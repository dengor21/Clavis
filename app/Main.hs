{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified System.HIDAPI as HID
import Data.Version (showVersion)
import Paths_Clavis (version)
import qualified Data.Map as M
import Keycodes (baseAliases)
import Formatter (formatLayoutMatrix)
import Data.Word (Word16)
import Data.Maybe
import Types
import Protocol
import Layout
import Options.Applicative

parseAction :: Parser Action
parseAction = subparser
  ( command "list" (info (pure List) (progDesc "List connected VIA keyboards"))
 <> command "yank" (info parseYank (progDesc "Read layout from keyboard"))
 <> command "put" (info parsePut (progDesc "Flash layout to keyboard"))
  )
  where
    parseYank = Yank
      <$> argument str (metavar "REF_JSON" <> help "Path to manufacturer VIA JSON")
      <*> argument str (metavar "LAYOUT_JSON" <> help "Path to output layout file")

    parsePut = Put
      <$> argument str (metavar "REF_JSON" <> help "Path to manufacturer VIA JSON")
      <*> argument str (metavar "LAYOUT_JSON" <> help "Path to layout file to flash")


clavisVersion :: String
clavisVersion = showVersion version

main :: IO ()
main = do
  cmd <- execParser optsInfo
  putStrLn $ "Clavis v" ++ clavisVersion
  exClav cmd
  where
    optsInfo = info (parseAction <**> helper)
      ( fullDesc
     <> progDesc "VIA Keyboard Layout manager"
     <> header ("Clavis v" ++ clavisVersion)
      )


withViaKeyboard :: FilePath -> (HID.Device -> ViaConfig -> IO ()) -> IO ()
withViaKeyboard refPath callback = do
  result <- loadJsonFile refPath
  case result of
    Left err -> putStrLn $ "Failed to parse the reference JSON: " ++ err
    Right config ->
      case (,) <$> parseHex (vendorId config) <*> parseHex (productId config) of
        Left err -> putStrLn $ "Failed to parse hardware ID: " ++ err
        Right (rawVid, rawPid) -> do
          let vid = VendorId rawVid
              pid = ProductId rawPid

          HID.withHIDAPI $ do
            connection <- connectViaKeyboard vid pid
            case connection of
              Nothing -> do
                putStrLn "Could not find VIA endpoint."
                putStrLn " -> Is the keyboard plugged in?"
                putStrLn " -> (Linux) Do you have the correct udev rules?"
              Just keyboard -> do
                success <- tryViaHandshake keyboard
                if success then do
                  putStrLn $ "Connected to " ++ name config ++ " successfully"
                  callback keyboard config
                else
                  putStrLn "Handshake failed"
                HID.close keyboard


exClav :: Action -> IO ()
exClav List = do
  executeList
exClav (Yank ref out) = withViaKeyboard ref $ \keyboard config ->
  executeYank keyboard config (buildReverseKeycodeDict config) out
exClav (Put ref input) = withViaKeyboard ref $ \keyboard config ->
  executePut keyboard config (buildForwardKeycodeDict config) input


-- | Reads the keyboard layout via hidapi and transforms it into a JSON
-- The JSON is transposed into layout resembling the physical keyboard
-- The information for this transformation is read from the VIA reference JSON
executeYank :: HID.Device -> ViaConfig -> M.Map Word16 String -> FilePath -> IO ()
executeYank keyboard config reverseDict layoutPath = do
  putStrLn "\nYanking layout from keyboard"

  case getLayoutMapping config of
    Left err -> putStrLn $ "Failed to read layout mapping: " ++ err
    Right mapping -> do
      rawLayers <- yankRawLayout keyboard mapping reverseDict
      let physicalLayers = toPhysicalLayout rawLayers mapping

      writeLayoutFile layoutPath physicalLayers
      putStrLn $ "\nSuccess! Layout yanked to '" ++ layoutPath ++ "'"
  

-- | Writes a previously generated layout JSON back to the keyboard
-- The JSON is transposed from the physical Layout back to the electrical grid.
-- Then executePut sends a set command to the keyboard for each of the keys.
executePut :: HID.Device -> ViaConfig -> M.Map String Word16 -> FilePath -> IO ()
executePut keyboard config forwardDict layoutPath = do
  putStrLn $ "\nReading layout from: " ++ layoutPath
  result <- loadJsonFile layoutPath

  case result of
    Left err -> putStrLn $ "Error parsing layout JSON: " ++ err
    Right userConfig -> do
        case getLayoutMapping config of
          Left err -> putStrLn $ "Failed to read layout mapping: " ++ err
          Right mapping -> do
            let electricalLayers = toElectricalLayout (layers userConfig) mapping

            putStrLn "Flashing layout to keyboard..."
            putLayout keyboard electricalLayers forwardDict
            putStrLn "\nSuccess! Your layout has been flashed to the keyboard"

executeList :: IO ()
executeList = do
  devices <- enumerateViaDevices
  mapM_ (\ dev -> putStrLn $ formatDeviceInfo dev) devices


loadJsonFile :: FromJSON a => FilePath -> IO (Either String a)
loadJsonFile path = do
  jsonBytes <- BSL.readFile path
  return (eitherDecode jsonBytes)


buildForwardKeycodeDict :: ViaConfig -> M.Map String Word16
buildForwardKeycodeDict config = case customKeycodes config of
  Just customList -> buildDict $ zip (map shortName customList) [0x7F00 ..]
  Nothing -> baseAliases


buildReverseKeycodeDict :: ViaConfig -> M.Map Word16 String
buildReverseKeycodeDict config = 
  let forwardDict = buildForwardKeycodeDict config
  in M.fromList [ (val, key) | (key, val) <- M.toList forwardDict ]


buildDict :: [(String, Word16)] -> M.Map String Word16
buildDict list = M.union (M.fromList list) baseAliases


writeLayoutFile :: FilePath -> [[[String]]] -> IO ()
writeLayoutFile layoutPath physicalLayers = do
  BSL8.writeFile layoutPath (formatLayoutMatrix physicalLayers)


formatDeviceInfo :: HID.DeviceInfo -> String
formatDeviceInfo device = 
  (fromMaybe "" (HID.productString device)) ++ "\n" 
    ++ "VendorID: " ++ show (HID.vendorId device) ++ " ProductID: " ++ show (HID.productId device) ++ "\n"
