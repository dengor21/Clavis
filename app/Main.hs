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
import Types
import Protocol
import Layout
import Options.Applicative

parseAction :: Parser Action
parseAction = subparser
  ( command "yank" (info (pure Yank) (progDesc "Read layout from keyboard"))
 <> command "put" (info (pure Put) (progDesc "Flash layout to keyboard"))
  )

parseOptions :: Parser Options
parseOptions = Options
  <$> parseAction
  <*> argument str (metavar "REF_JSON" <> help "Path to manufacturer VIA JSON")
  <*> argument str (metavar "LAYOUT_JSON" <> help "Path to layout file")


clavisVersion :: String
clavisVersion = showVersion version

main :: IO ()
main = do
  opts <- execParser optsInfo
  putStrLn $ "Clavis v" ++ clavisVersion 
  executeClavis (optAction opts) (optRefPath opts) (optLayoutPath opts)
  where
    optsInfo = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "VIA Keyboard Layout flasher"
     <> header ("Clavis v" ++ clavisVersion)
      )

-- | Executes one complete program run
-- at this time, this is either reading or writing the layout
executeClavis :: Action -> FilePath -> FilePath -> IO ()
executeClavis act refPath layoutPath = do
  result <- loadJsonFile refPath
  case result of
    Left err -> putStrLn $ "Failed to parse reference JSON: " ++ err
    Right config ->
      case (,) <$> parseHex (vendorId config) <*> parseHex (productId config) of
        Left err -> putStrLn $ "Failed to parse hardware ID: " ++ err
        Right (rawVid, rawPid) -> do
          let vid = VendorId rawVid
              pid = ProductId rawPid
              (forwardDict, reverseDict) = buildCustomKeycodeDicts config

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
                  case act of
                    Yank -> executeYank keyboard config reverseDict layoutPath
                    Put -> executePut keyboard config forwardDict layoutPath
                else
                  putStrLn "Handshake failed"
                HID.close keyboard

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


loadJsonFile :: FromJSON a => FilePath -> IO (Either String a)
loadJsonFile path = do
  jsonBytes <- BSL.readFile path
  return (eitherDecode jsonBytes)

-- | Builds two dictionaries out of the custom keycodes defined in the reference JSON
-- If there are none, empty dictionaries are returned
buildCustomKeycodeDicts :: ViaConfig -> (M.Map String Word16, M.Map Word16 String)
buildCustomKeycodeDicts config =
  let dynamicCodes = case customKeycodes config of
        Just customList -> zip (map shortName customList) [0x7F00 ..]
        Nothing -> []
      forwardDict = M.union (M.fromList dynamicCodes) baseAliases
      reverseDict = M.fromList [ (val, key) | (key, val) <- M.toList forwardDict ]
  in (forwardDict, reverseDict)


writeLayoutFile :: FilePath -> [[[String]]] -> IO ()
writeLayoutFile layoutPath physicalLayers = do
  BSL8.writeFile layoutPath (formatLayoutMatrix physicalLayers)

