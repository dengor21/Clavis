{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Json where

import Types
import Keycodes
import Data.Aeson
import Control.Applicative
import Data.Word
import Data.Aeson.Types (Parser)

instance FromJSON CustomKeycode
instance FromJSON ViaConfig
instance FromJSON UserConfig
instance FromJSON LayerData

instance FromJSON MacroAction where
  parseJSON = withObject "Macro" $ \obj -> do
    let tryText = MacroText <$> obj .: "text"
        tryTap = do
          mname <- obj .: "tap"
          parseKeycode mname MacroTap
        tryRelease = do
          mname <- obj .: "release"
          parseKeycode mname MacroRelease
        tryPress = do
          mname <- obj .: "press"
          parseKeycode mname MacroPress
        tryDelay = MacroDelay <$> obj .: "delay"
    tryText <|> tryTap <|> tryRelease <|> tryPress <|> tryDelay
    
instance FromJSON Macro where
  parseJSON = withObject "Macro" $ \obj -> do
    mid     <- obj .: "id"
    actions <- obj .: "actions"
    return (Macro mid actions)


parseKeycode :: String -> (Word8 -> MacroAction) -> Parser MacroAction
parseKeycode keyname constructor =
  case translateToHex keyname of
    Left _ -> fail "unknown keycode"
    Right code
      | code > 0xFF -> fail "macros only support basic keycodes"
      | otherwise   -> return $ constructor (fromIntegral code)
