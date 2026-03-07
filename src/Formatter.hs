module Formatter (formatLayoutMatrix) where

import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Types (LayerData(..), UserConfig(..))

-- | Inserts whitespaces and newlines into the JSON
-- For readability
formatLayoutMatrix :: UserConfig -> BSL8.ByteString
formatLayoutMatrix uc  =
    BSL8.pack $ ""
    ++ "{\n"
    ++ "  \"formatVersion\": " ++ show (formatVersion uc) ++ ", \n"
    ++ "  \"keyboardName\": \"" ++ keyboardName uc ++ "\", \n"
    ++ "  \"vid\": \"" ++ vid uc ++ "\", \n"
    ++ "  \"pid\": \"" ++ pid uc ++ "\", \n"
    ++ "  \"macros\": [],\n"
    ++ "  \"layers\": [\n" 
    ++ formatLayers (layers uc)
    ++ "\n  ]\n}\n"
  where
    formatLayers ls = intercalate ",\n" (map formatLayer ls)
    formatLayer (LayerData ln rs) =  
         "    {\n" 
      ++ "      \"layer\": "  ++ show ln ++ ",\n" 
      ++ "      \"keys\": [\n"
      ++ intercalate ",\n" (map formatRow rs) 
      ++ "\n      ]\n    }"
    formatRow cs =    "        [" ++ intercalate ", " (map show cs) ++ "]"
