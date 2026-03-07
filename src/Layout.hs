{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Layout where

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Data.Word (Word8, Word16)
import Data.Aeson (Value(Object, Array, String))
import Data.Bits (shiftL, (.|.))
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Numeric (readHex)
import Types

parseHex :: String -> Either String Word16
parseHex str = 
    let cleanStr = if take 2 str == "0x" then drop 2 str else str
    in case readHex cleanStr of
        [(val, "")] -> Right val
        _           -> Left $ "Could not parse hex string: " ++ str

-- Decode the keyboard's 32-byte response into a 16-bit word
parseKeycodeResponse :: [Word8] -> Word16
parseKeycodeResponse (0x04 : _ : _ : _ : high : low : _) =
    let high16 = (fromIntegral high :: Word16) `shiftL` 8
        low16  = fromIntegral low :: Word16
    in high16 .|. low16
parseKeycodeResponse _ = 0x0000 -- Fallback for empty or malformed reads

getLayoutMapping :: ViaConfig -> Either String [[(Int, Int)]]
getLayoutMapping config =
  case layouts config of
    Object obj | Just (Array kleRows) <- KM.lookup "keymap" obj ->
      Right $ map parseKleRow (toList kleRows)
    _ -> 
      Left $ "Error: Could not find 'keymap' array. Is this a VIA V3 JSON?"

-- | Parses a single physical row in the KLE array
parseKleRow :: Value -> [(Int, Int)]
parseKleRow (Array cols) = 
  let strVals = [ T.unpack s | String s <- toList cols ]
  in  mapMaybe parseCoordinate strVals
parseKleRow _ = []

-- | Extracts the (Row, Col) tuple from a KLE string like "0,1\nLabel"
parseCoordinate :: String -> Maybe (Int, Int)
parseCoordinate str =
  let cleanStr = takeWhile (/= '\n') str
      parts = words (map (\c -> if c == ',' then ' ' else c) cleanStr)
  in case parts of
       (rStr:cStr:_) -> 
         -- Using 'reads' safely ignores non-coordinate strings like "Esc"
         case (reads rStr :: [(Int, String)], reads cStr :: [(Int, String)]) of
           ((r, _):_, (c, _):_) -> Just (r, c)
           _                    -> Nothing
       _ -> Nothing  

toPhysicalLayout :: [[[String]]] -> [[(Int, Int)]] -> [[[String]]]
toPhysicalLayout rawLayers mapping = 
  [ [ [ rawLayer !! r !! c | (r, c) <- rowMapping ]
    | rowMapping <- mapping
    ]
  | rawLayer <- rawLayers
  ]


toPhysicalLayoutSingle :: [[String]] -> [[(Int, Int)]] -> [[String]]
toPhysicalLayoutSingle rawLayer mapping =
  [ [ rawLayer !! r !! c | (r, c) <- rowMapping ]
  | rowMapping <- mapping
  ]

toElectricalLayer :: [[String]] -> [[(Int, Int)]] -> [[String]]
toElectricalLayer physicalLayer mapping =
  let flatMapping = concat mapping
      maxR = maximum (map fst flatMapping)
      maxC = maximum (map snd flatMapping)
      blankMatrix = replicate (maxR + 1) (replicate (maxC + 1) "none")
  in foldl updateMatrix blankMatrix (zip flatMapping (concat physicalLayer))

generateCoordinateList :: Word8 -> [(Int, Int)] -> [(Layer, Row, Col)]
generateCoordinateList numLayers flatMapping =
  let numRows = fromIntegral (maximum (map fst flatMapping) + 1) :: Word8
      numCols = fromIntegral (maximum (map snd flatMapping) + 1) :: Word8

  in  [ (Layer l, Row r, Col c) | l <- [0 .. numLayers -1]
                                , r <- [0 .. numRows -1]
                                , c <- [0 .. numCols -1] ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

updateMatrix :: [[String]] -> ((Int, Int), String) -> [[String]]
updateMatrix mat ((r, c), val) =
  replaceAt r (replaceAt c val (mat !! r)) mat
