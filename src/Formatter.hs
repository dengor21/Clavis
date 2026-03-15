module Formatter (formatLayoutMatrix) where

import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Types (Macro(..), MacroAction(..), LayerData(..), UserConfig(..))
import Data.Word (Word16)
import Numeric (showHex)
import Keycodes (reverseAliases)
import qualified Data.Map as M

formatLayoutMatrix :: UserConfig -> BSL8.ByteString
formatLayoutMatrix uc =
    BSL8.pack $ ""
    ++ "{\n"
    ++ "  \"formatVersion\": " ++ show (formatVersion uc) ++ ",\n"
    ++ "  \"keyboardName\": \"" ++ keyboardName uc ++ "\",\n"
    ++ "  \"vid\": \"" ++ vid uc ++ "\",\n"
    ++ "  \"pid\": \"" ++ pid uc ++ "\",\n"
    ++ "  \"layers\": [\n"
    ++ formatLayers (layers uc)
    ++ "\n  ],\n"
    ++ "  \"macros\": [\n"
    ++ formatMacros (macros uc)
    ++ "\n  ]\n}\n"
  where
    formatLayers ls = intercalate ",\n" (map formatLayer ls)
    formatLayer (LayerData ln rs) =
         "    {\n"
      ++ "      \"layer\": " ++ show ln ++ ",\n"
      ++ "      \"keys\": [\n"
      ++ intercalate ",\n" (map formatRow rs)
      ++ "\n      ]\n    }"
    formatRow cs = "        [" ++ intercalate ", " (map show cs) ++ "]"

    formatMacros ms = intercalate ",\n" (map formatMacro ms)
    formatMacro (Macro mid actions) =
         "    {\n"
      ++ "      \"id\": " ++ show mid ++ ",\n"
      ++ "      \"actions\": ["
      ++ if null actions
          then "]" ++ "\n    }"
            else "\n" ++ intercalate ",\n" (map formatAction actions) ++ "\n      ]" ++ "\n    }"

    formatAction (MacroText t) =
      "        {\"text\": " ++ show t ++ "}"
    formatAction (MacroTap k) =
      "        {\"tap\": \"" ++ keycodeToName k ++ "\"}"
    formatAction (MacroPress k) =
      "        {\"press\": \"" ++ keycodeToName k ++ "\"}"
    formatAction (MacroRelease k) =
      "        {\"release\": \"" ++ keycodeToName k ++ "\"}"
    formatAction (MacroDelay ms) =
      "        {\"delay\": " ++ show ms ++ "}"

    keycodeToName k =
      let k16 = fromIntegral k :: Word16
      in case M.lookup k16 reverseAliases of
           Just name -> name
           Nothing   -> "0x" ++ showHex k ""
