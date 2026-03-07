{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import Types (ViaCommand(..), Keycode(..), Layer(..), Row(..), Col(..), VendorId(..), ProductId(..), LayerData(..) )
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Word (Word8, Word16)
import Data.Bits (shiftR, (.&.))
import qualified System.HIDAPI as HID
import Data.List (find)
import Control.Monad (forM, forM_)
import Control.Concurrent (threadDelay)
import Numeric (showHex)
import Layout
import Keycodes

viaUsagePage :: Word16
viaUsagePage = 65376

viaMessageLength :: Int
viaMessageLength = 32

timeoutInMs :: Int
timeoutInMs = 1000

writeDelayInMs :: Int
writeDelayInMs = 2000


yankRawLayout :: HID.Device -> [[(Int, Int)]] -> M.Map Word16 String -> IO [[[String]]]
yankRawLayout keyboard mapping reverseDict = do
  let flatMapping = concat mapping
      numRows = maximum (map fst flatMapping) + 1
      numCols = maximum (map snd flatMapping) + 1

  layerCount <- queryLayerCount keyboard
  results <- forM (generateCoordinateList layerCount flatMapping) $ \(l, r, c) ->
    readKeycode keyboard reverseDict l r c

  let rows = chunksOf numCols results
      layerData = chunksOf numRows rows

  return layerData


yankRawLayoutN :: HID.Device -> [[(Int, Int)]] -> M.Map Word16 String -> Word8 -> IO (Maybe [[String]])
yankRawLayoutN keyboard mapping reverseDict la = do
  let flatMapping = concat mapping
      numCols = maximum (map snd flatMapping) + 1 
      maxCols = fromIntegral (numCols - 1) :: Word8
      maxRows = fromIntegral $ maximum (map fst flatMapping) :: Word8

      coords = [ (Row r, Col c) | r <- [0 .. maxRows]
                                , c <- [0 .. maxCols]]
  layerCount <- queryLayerCount keyboard

  if la >= layerCount
    then return Nothing
    else do
      results <- forM coords $ \ (r, c) ->
        readKeycode keyboard reverseDict (Layer la) r c

      return $ Just (chunksOf numCols results)


putLayout :: HID.Device -> [[[String]]] -> M.Map String Word16 -> IO ()
putLayout keyboard rawLayers forwardDict = do
  forM_ (zip ([0..] :: [Int]) rawLayers) $ \(l, rowList) ->
    forM_ (zip ([0..] :: [Int]) rowList) $ \(r, colList) ->
      forM_ (zip ([0..] :: [Int]) colList) $ \(c, keyStr) ->
        case keyStr of
          "none" -> return ()
          _ -> do
            let hexResult = case M.lookup keyStr forwardDict of
                  Just val -> Right val
                  Nothing  -> parseHexFallback keyStr
            case hexResult of
              Left err -> putStrLn $ "Warning: Skipping key at (" ++ show l ++ "," ++ show r ++ "," ++ show c ++ "): " ++ err
              Right hexVal -> do
                let cmd = serializeViaCmd (SetKeycode
                      (Layer $ fromIntegral l)
                      (Row $ fromIntegral r)
                      (Col $ fromIntegral c)
                      (Keycode hexVal))
                _ <- HID.write keyboard cmd
                threadDelay writeDelayInMs


putLayoutN :: HID.Device -> [[String]] -> Word8 -> M.Map String Word16 -> IO ()
putLayoutN keyboard rawLayer la forwardDict = do
  forM_ (zip ([0..] :: [Int]) rawLayer) $ \(r, colList) ->
    forM_ (zip ([0..] :: [Int]) colList) $ \(c, keyStr) ->
      case keyStr of
        "none" -> return ()
        _ -> do
          let hexResult = case M.lookup keyStr forwardDict of
                Just val -> Right val
                Nothing  -> parseHexFallback keyStr

          case hexResult of
            Left err -> putStrLn $ "Warning: Skipping key at (" ++ show la ++ "," ++ show r ++ "," ++ show c ++ "): " ++ err
            Right hexVal -> do
              let cmd = serializeViaCmd (SetKeycode
                    (Layer la)
                    (Row $ fromIntegral r)
                    (Col $ fromIntegral c)
                    (Keycode hexVal))
              _ <- HID.write keyboard cmd
              threadDelay writeDelayInMs 



-- | Serializes the ViaCommand and its parameters to the corresponding bytes
serializeViaCmd :: ViaCommand -> BS.ByteString
serializeViaCmd cmd = BS.pack $ padTo32 (toBytes cmd)
  where
    toBytes GetProtocolVersion = [0x01]
    toBytes (GetKeycode (Layer l) (Row r) (Col c)) = [0x04, l, r, c]
    toBytes (SetKeycode (Layer l) (Row r) (Col c) (Keycode keycode)) = 
        [ 0x05, l, r, c
        , fromIntegral (keycode `shiftR` 8)  -- High byte
        , fromIntegral (keycode .&. 0xFF)    -- Low byte
        ]
    toBytes GetLayerCount = [0x11]
    padTo32 bytes = take 32 (bytes ++ repeat 0x00)


readKeycode :: HID.Device -> M.Map Word16 String -> Layer -> Row -> Col -> IO String
readKeycode keyboard reverseDict layer row col = do
  let cmd = serializeViaCmd (GetKeycode layer row col)
  result <- HID.write keyboard cmd

  case result of
    -1 -> return "Err"
    _ -> do
      response <- HID.readTimeout keyboard viaMessageLength timeoutInMs
      threadDelay writeDelayInMs
      let hexVal = parseKeycodeResponse (BS.unpack response)
      return $ case M.lookup hexVal reverseDict of
        Just alias -> alias
        Nothing -> "0x" ++ showHex hexVal ""


connectViaKeyboard :: VendorId -> ProductId -> IO (Maybe HID.Device)
connectViaKeyboard (VendorId vid) (ProductId pid) = do 
  devices <- HID.enumerate (Just vid) (Just pid)
  let viaEndpoint = find (\d -> HID.usagePage d == viaUsagePage) devices
  traverse (HID.openPath . HID.path) viaEndpoint


tryViaHandshake :: HID.Device -> IO Bool
tryViaHandshake keyboard = do
  result <- HID.write keyboard (serializeViaCmd GetProtocolVersion)

  case result of
        -1 -> return False
        _ -> do
          _ <- HID.readTimeout keyboard timeoutInMs viaMessageLength
          return True


queryLayerCount :: HID.Device -> IO Word8
queryLayerCount keyboard = do
  let cmd = serializeViaCmd GetLayerCount
  result <- HID.write keyboard cmd
  case result of
    -1 -> return 4
    _ -> do
      response <- HID.readTimeout keyboard viaMessageLength timeoutInMs
      return $ parseLayerCountResponse (BS.unpack response)


parseLayerCountResponse :: [Word8] -> Word8
parseLayerCountResponse (0x11 : n : _) = n
parseLayerCountResponse _ = 4

isViaDevice :: HID.DeviceInfo -> Bool
isViaDevice d = HID.usagePage d == viaUsagePage 

enumerateViaDevices :: IO [HID.DeviceInfo]
enumerateViaDevices = do
  devices <- HID.enumerate Nothing Nothing
  let viaDevices = filter isViaDevice devices
  return viaDevices 
