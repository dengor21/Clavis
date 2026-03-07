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
import Data.Word (Word8, Word16)
import Data.Maybe
import Types
import Protocol
import Layout
import Options.Applicative
import Control.Monad (when, forM_)

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
      <*> optional (option auto (long "layer" <> short 'l' <> metavar "N" <> help "Layer number"))

    parsePut = Put
      <$> argument str (metavar "REF_JSON" <> help "Path to manufacturer VIA JSON")
      <*> argument str (metavar "LAYOUT_JSON" <> help "Path to layout file to flash")
      <*> optional (option auto (long "layer" <> short 'l' <> metavar "N" <> help "Layer number"))


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
          let hexvid = VendorId rawVid
              hexpid = ProductId rawPid

          HID.withHIDAPI $ do
            connection <- connectViaKeyboard hexvid hexpid
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
exClav (Yank ref out la) = withViaKeyboard ref $ \keyboard config ->
  executeYank keyboard config (buildReverseKeycodeDict config) la out
exClav (Put ref input la) = withViaKeyboard ref $ \keyboard config ->
  executePut keyboard config (buildForwardKeycodeDict config) la input


-- | Reads the keyboard layout via hidapi and transforms it into a JSON
-- The JSON is transposed into layout resembling the physical keyboard
-- The information for this transformation is read from the VIA reference JSON
executeYank :: HID.Device -> ViaConfig -> M.Map Word16 String -> Maybe Word8 -> FilePath -> IO ()
executeYank keyboard config reverseDict maybeLayer layoutPath = do
  putStrLn "\nYanking layout from keyboard"

  case getLayoutMapping config of
    Left err -> putStrLn $ "Failed to read layout mapping: " ++ err
    Right mapping -> 
      case maybeLayer of
        Nothing -> do
          rawLayers <- yankRawLayout keyboard mapping reverseDict
          let physicalLayers = toPhysicalLayout rawLayers mapping
              layerDataList = zipWith (\n keyData -> LayerData n keyData)
                                [0..] physicalLayers
          writeLayoutFile layoutPath (createUserConfig layerDataList config)
          putStrLn $ "\nSuccess! Layout yanked to '" ++ layoutPath ++ "'"

        Just n -> do
          result <- yankRawLayoutN keyboard mapping reverseDict n
          case result of
            Nothing -> putStrLn $ "Layer " ++ show n ++ " does not exist"
            Just rawLayer -> do
              let physicalLayer = toPhysicalLayoutSingle rawLayer mapping
                  layerDataList = [LayerData n physicalLayer]
              writeLayoutFile  layoutPath  (createUserConfig layerDataList config)
    where
      createUserConfig layerList (ViaConfig kbName venId prodId _ _) = 
        UserConfig 1 kbName venId prodId [] layerList 

            
  

-- | Writes a previously generated layout JSON back to the keyboard
-- The JSON is transposed from the physical Layout back to the electrical grid.
-- Then executePut sends a set command to the keyboard for each of the keys.
executePut :: HID.Device -> ViaConfig -> M.Map String Word16 -> Maybe Word8 -> FilePath -> IO ()
executePut keyboard config forwardDict maybeLayer layoutPath = do
  putStrLn $ "\nReading layout from: " ++ layoutPath
  result <- loadJsonFile layoutPath

  case result of
    Left err -> putStrLn $ "Error parsing layout JSON: " ++ err
    Right userConfig -> 
      case validateConfig userConfig (vendorId config) (productId config) of
        Left err -> putStrLn err
        Right () ->
          case maybeLayer of
            Nothing -> do
              case getLayoutMapping config of
                Left err -> putStrLn $ "Failed to read layout mapping: " ++ err
                Right mapping -> do
                  putStrLn "Flashing layout to keyboard..."
                  forM_ (layers userConfig) $ \ld ->
                    let electrical = toElectricalLayer (keys ld) mapping
                    in putLayoutN keyboard electrical (layer ld) forwardDict
                  putStrLn "\nSuccess! Your layout has been flashed to the keyboard"

            Just n -> do
              case getLayoutMapping config of
                Left err -> putStrLn $ "Failed to read layout mapping: " ++ err
                Right mapping -> do
                  putStrLn "Flashing layout to keyboard..."
                  let electrical = toElectricalLayer (keys $ (layers userConfig ) !! fromIntegral n) mapping
                  putLayoutN keyboard electrical n forwardDict


validateConfig :: UserConfig -> String -> String -> Either String ()
validateConfig config rawVid rawPid = do
  when (formatVersion config > 1) $
    Left "Hello to the future, it seems I had to revise the format. \nPlease get the latest version or yank a new layout file"
  when (vid config /= rawVid) $
    Left "VendorIDs don't match." 
  when (pid config /= rawPid) $
    Left "ProductIDs don't match."


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


writeLayoutFile :: FilePath -> UserConfig -> IO ()
writeLayoutFile layoutPath uc = do
  BSL8.writeFile layoutPath (formatLayoutMatrix uc)


formatDeviceInfo :: HID.DeviceInfo -> String
formatDeviceInfo device = 
  (fromMaybe "" (HID.productString device)) ++ "\n" 
    ++ "VendorID: " ++ show (HID.vendorId device) ++ " ProductID: " ++ show (HID.productId device) ++ "\n"
