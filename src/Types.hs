{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)

newtype Layer = Layer Word8 deriving (Show, Eq)
newtype Row = Row Word8 deriving (Show, Eq)
newtype Col = Col Word8 deriving (Show, Eq)
newtype Keycode = Keycode Word16 deriving (Show, Eq)
newtype LayerCount = LayerCount Word8 deriving (Show, Eq)

newtype VendorId = VendorId Word16 deriving (Show, Eq)
newtype ProductId = ProductId Word16 deriving (Show, Eq)

-- | Represents one manufacturer defined custom keycode
data CustomKeycode = CustomKeycode
  { shortName :: String -- ^ Short name used as alias for keycode
  } deriving (Show, Generic)

instance FromJSON CustomKeycode

-- | This data structure mirrors the VIA/QMK JSON reference file your manufacturer provides
-- clavis relies on this data to produce a close physical layout and to map custom keycode aliases
data ViaConfig = ViaConfig
  { name      :: String -- ^ Keyboard model name
  , vendorId  :: String -- ^ Hardware vendor id
  , productId :: String -- ^ Hardware product id
  , customKeycodes :: Maybe [CustomKeycode] -- ^ List of manufacturer defined custom keycodes
  , layouts :: Value -- ^ VIA V3 uses an raw array
  } deriving (Show, Generic)

instance FromJSON ViaConfig

-- | This data structure mirrors the user layout config file.
-- clavis tranposes the raw electrical layout to one resembling the physical layout of your keyboard.
-- The accuracy of this depends on the quality of the reference JSON your manufacturer provides.
data UserConfig = UserConfig 
  { layers :: [[[String]]] -- ^ The layers containing the layout 
  } deriving (Show, Generic)

instance FromJSON UserConfig
instance ToJSON UserConfig

-- | Command data structure.
-- clavis supports the handshake and then only getting and setting keycodes.
-- These commands are translated to hex codes and later to the corresponding bytes.
data ViaCommand 
    = GetProtocolVersion                -- ^ [0x01] Handshake and protocol information
    | GetKeycode Layer Row Col          -- ^ [0x04] Gets the keycode at the specified position
    | SetKeycode Layer Row Col Keycode  -- ^ [0x05] Sets the provided keycode at the provided position
    | GetLayerCount                     -- ^ [0x11] Gets the count of layout layers

-- | Represents the actions Clavis can execute
data Action 
  = Yank -- ^ Reads the layout from the keyboard
  | Put -- ^ Writes the layout to the keyboard

data Options = Options
  { optAction :: Action
  , optRefPath :: FilePath
  , optLayoutPath :: FilePath
  }
