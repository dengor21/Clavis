module Keycodes (translateToString, translateToHex, parseHexFallback, baseAliases, reverseAliases) where

import Data.Word (Word16)
import Numeric (readHex, showHex)
import qualified Data.Map as M

-- | The master dictionary mapping human-readable strings to 16-bit VIA Keycodes
baseAliases :: M.Map String Word16
baseAliases = M.fromList
    ( [ -- Basic Letters
      ("a", 0x0004), ("b", 0x0005), ("c", 0x0006), ("d", 0x0007)
    , ("e", 0x0008), ("f", 0x0009), ("g", 0x000A), ("h", 0x000B)
    , ("i", 0x000C), ("j", 0x000D), ("k", 0x000E), ("l", 0x000F)
    , ("m", 0x0010), ("n", 0x0011), ("o", 0x0012), ("p", 0x0013)
    , ("q", 0x0014), ("r", 0x0015), ("s", 0x0016), ("t", 0x0017)
    , ("u", 0x0018), ("v", 0x0019), ("w", 0x001A), ("x", 0x001B)
    , ("y", 0x001C), ("z", 0x001D)
    
      -- Numbers
    , ("1", 0x001E), ("2", 0x001F), ("3", 0x0020), ("4", 0x0021)
    , ("5", 0x0022), ("6", 0x0023), ("7", 0x0024), ("8", 0x0025)
    , ("9", 0x0026), ("0", 0x0027)

      -- Numpad
    , ("NumLock", 0x53)
    
      -- Punctuation & Navigation
    , ("enter", 0x0028), ("esc", 0x0029), ("bspace", 0x002A), ("tab", 0x002B)
    , ("spc", 0x002C), ("-", 0x002D), ("=", 0x002E), ("[", 0x002F)
    , ("]", 0x0030), ("\\", 0x0031), (";", 0x0033), ("'", 0x0034)
    , ("`", 0x0035), (",", 0x0036), (".", 0x0037), ("/", 0x0038)
    , ("caps", 0x39)
    
      -- F-Keys
    , ("f1", 0x003A), ("f2", 0x003B), ("f3", 0x003C), ("f4", 0x003D)
    , ("f5", 0x003E), ("f6", 0x003F), ("f7", 0x0040), ("f8", 0x0041)
    , ("f9", 0x0042), ("f10", 0x0043), ("f11", 0x0044), ("f12", 0x0045)
    
      -- Control Pad & Arrows
    , ("prtsc", 0x0046), ("sclk", 0x0047), ("pause", 0x0048), ("ins", 0x0049)
    , ("home", 0x004A), ("pgup", 0x004B), ("del", 0x004C), ("end", 0x004D)
    , ("pgdn", 0x004E), ("right", 0x004F), ("left", 0x0050), ("down", 0x0051)
    , ("up", 0x0052)

      -- Modifiers (We include Mac and PC aliases, but reverse-mapping will favor the last one)
    , ("lctl", 0x00E0), ("lsft", 0x00E1), ("lalt", 0x00E2), ("lopt", 0x00E2)
    , ("lgui", 0x00E3), ("lcmd", 0x00E3), ("rctl", 0x00E4), ("rsft", 0x00E5)
    , ("ralt", 0x00E6), ("ropt", 0x00E6), ("rgui", 0x00E7), ("rcmd", 0x00E7)
    
      -- Media Keys
    , ("mute", 0x00A8), ("volu", 0x00A9), ("vold", 0x00AA)
    
      -- VIA/QMK Specifics
    , ("none", 0x0000), ("trans", 0x0001)
    
    -- Media and Display Brightness
    , ("play", 0x00AE), ("mprv", 0x00AC), ("mnxt", 0x00AB)
    , ("briu", 0x00BE), ("brid", 0x00BD)
    
      -- VIA/QMK Standard RGB Lighting Controls
    , ("rgb_tog", 0x7820), ("rgb_mod", 0x7821), ("rgb_rmod", 0x7822)
    , ("rgb_hui", 0x7823), ("rgb_hud", 0x7824), ("rgb_sai", 0x7825)
    , ("rgb_sad", 0x7826), ("rgb_vai", 0x7827), ("rgb_vad", 0x7828)
    , ("rgb_spi", 0x7829), ("rgb_spd", 0x782A)
    
      -- Keychron Dedicated Macro Keys (M-keys)
    , ("macro(0)", 0x7700), ("macro(1)", 0x7701), ("macro(2)", 0x7702)
    , ("macro(3)", 0x7703), ("macro(4)", 0x7704), ("macro(5)", 0x7705)
    , ("macro(6)", 0x7706)
    
      -- Keychron Layer Toggles (Mac Fn and Win Fn)
    , ("fn1", 0x5221), ("fn2", 0x5222), ("fn3", 0x5223)
    
      -- QMK System/Hardware controls 
      -- (0x7013 often acts as a hardware reset/sleep toggle on Keychron splits)
    , ("sys_misc", 0x7013)
    ])

-- | Automatically build the reverse dictionary for the 'yank' command
reverseAliases :: M.Map Word16 String
reverseAliases = M.fromList [ (val, key) | (key, val) <- M.toList baseAliases ]

-- | Convert keyboard memory into user-friendly strings
translateToString :: Word16 -> String
translateToString hexVal =
    case M.lookup hexVal reverseAliases of
        Just strAlias -> strAlias
        -- If we don't have it in the dictionary, output the raw hex (e.g., "0x00E3")
        Nothing       -> "0x" ++ showHex hexVal "" 

-- | Convert user-friendly strings back into keyboard memory
translateToHex :: String -> Either String Word16
translateToHex str = 
    case M.lookup str baseAliases of
        Just hexVal -> Right hexVal
        -- Fallback: If the user typed "0x00E3", safely parse it back to a number
        Nothing     -> parseHexFallback str

-- | Safely parse hex strings if the user provides raw memory addresses
parseHexFallback :: String -> Either String Word16
parseHexFallback str = 
    let cleanStr = if take 2 str == "0x" then drop 2 str else str
    in case readHex cleanStr of
        [(val, "")] -> Right val
        _           -> Left $ "Invalid keycode or hex string in layout: " ++ str
