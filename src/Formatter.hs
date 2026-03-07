module Formatter (formatLayoutMatrix) where

import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Types (LayerData(..))

-- | Inserts whitespaces and newlines into the JSON
-- For readability
formatLayoutMatrix :: [LayerData] -> BSL8.ByteString
formatLayoutMatrix nlayer  =
    BSL8.pack $ "{\n \"layers\":\n  [\n" ++ formatLayers nlayer ++ "\n  ]\n}\n"
  where
    formatLayers ls = intercalate ",\n" (map formatLayer ls)
    formatLayer (LayerData ln rs) =  
         "    {\n" 
      ++ "      \"layer\": "  ++ show ln ++ " ,\n" 
      ++ "      \"keys\":\n     [\n"
      ++ intercalate ",\n" (map formatRow rs) 
      ++ "\n      ]\n   }"
    formatRow cs =    "      [" ++ intercalate ", " (map show cs) ++ "]"
