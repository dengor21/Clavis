{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (Value)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)

newtype Layer = Layer Word8 deriving (Show, Eq)
newtype Row = Row Word8 deriving (Show, Eq)
newtype Col = Col Word8 deriving (Show, Eq)
newtype Keycode = Keycode Word16 deriving (Show, Eq)
newtype LayerCount = LayerCount Word8 deriving (Show, Eq)

newtype VendorId = VendorId Word16 deriving (Show, Eq)
newtype ProductId = ProductId Word16 deriving (Show, Eq)
newtype MacroOffset = MacroOffset Word16 deriving (Show, Eq)
newtype MacroLength = MacroLength Word8 deriving (Show, Eq)

-- | Represents one manufacturer defined custom keycode
data CustomKeycode = CustomKeycode
  { shortName :: String -- ^ Short name used as alias for keycode
  } deriving (Show, Generic)


-- | This data structure mirrors the VIA/QMK JSON reference file your manufacturer provides
-- clavis relies on this data to produce a close physical layout and to map custom keycode aliases
data ViaConfig = ViaConfig
  { name            :: String                 -- ^ Keyboard model name
  , vendorId        :: String                 -- ^ Hardware vendor id
  , productId       :: String                 -- ^ Hardware product id
  , customKeycodes  :: Maybe [CustomKeycode]  -- ^ List of manufacturer defined custom keycodes
  , layouts         :: Value                  -- ^ VIA V3 uses an raw array
  } deriving (Show, Generic)

-- | This data structure mirrors the user layout config file.
-- clavis tranposes the raw electrical layout to one resembling the physical layout of your keyboard.
-- The accuracy of this depends on the quality of the reference JSON your manufacturer provides.
data UserConfig = UserConfig 
  { formatVersion   :: Int
  , keyboardName    :: String
  , vid             :: String
  , pid             :: String
  , macros          :: [Macro]
  , layers          :: [LayerData] -- ^ The layers containing the layout 
  } deriving (Show, Generic)


data LayerData = LayerData
  { layer   :: Word8
  , keys    :: [[String]]
  } deriving (Show, Generic)


data MacroAction
  = MacroText String
  | MacroTap Word8
  | MacroPress Word8
  | MacroRelease Word8
  | MacroDelay Int
  deriving (Show, Generic)


data Macro = Macro
  { macroId       :: Word8
  , macroActions  :: [MacroAction]
  } deriving (Show, Generic)

-- | Command data structure.
-- clavis supports the handshake and then only getting and setting keycodes.
-- These commands are translated to hex codes and later to the corresponding bytes.
data ViaCommand 
    = GetProtocolVersion                      -- ^ [0x01] Handshake and protocol information
    | GetKeycode Layer Row Col                -- ^ [0x04] Gets the keycode at the specified position
    | SetKeycode Layer Row Col Keycode        -- ^ [0x05] Sets the provided keycode at the provided position
    | GetLayerCount                           -- ^ [0x11] Gets the count of layout layers
    | GetMacroCount                           -- ^ [0x0E] Gets the number of macro slots 
    | GetMacroBufferSize                      -- ^ [0x0F] Get the total macro buffer size in bytes
    | GetMacroBytes Word16 Word8              -- ^ [0x0C] Gets a macro at the offset of specified length
    | SetMacroBytes Word16 Word8 [Word8]      -- ^ [0x0D] Sets a macro at the offset of specified length

-- | Represents the actions Clavis can execute
data Action 
  = Yank FilePath FilePath (Maybe Word8)  -- ^ Reads the layout from the keyboard
  | Put FilePath FilePath (Maybe Word8)   -- ^ Writes the layout to the keyboard
  | List

data Options = Options
  { optAction     :: Action
  , optRefPath    :: FilePath
  , optLayoutPath :: FilePath
  }
