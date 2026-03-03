module Formatter (formatLayoutMatrix) where

import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- | Inserts whitespaces and newlines into the JSON
-- For readability
formatLayoutMatrix :: [[[String]]] -> BSL8.ByteString
formatLayoutMatrix lys =
    BSL8.pack $ "{\n \"layers\":\n  [\n" ++ formatLayers lys ++ "\n  ]\n}\n"
  where
    formatLayers ls = intercalate ",\n" (map formatLayer ls)
    formatLayer rs =  "    [\n" ++ intercalate ",\n" (map formatRow rs) ++ "\n    ]"
    formatRow cs =    "      [" ++ intercalate ", " (map show cs) ++ "]"
