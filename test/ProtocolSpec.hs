module ProtocolSpec (spec) where

import Test.Hspec
import Protocol
import Types
import qualified Data.ByteString as BS

spec :: Spec
spec = do
  describe "Protocol command serialization (serializeViaCmd)" $ do
    
    it "serializes GetProtocolVersion (0x01) with 31 bytes of padding" $ do
      let expected = BS.pack $ 0x01 : replicate 31 0x00
      serializeViaCmd GetProtocolVersion `shouldBe` expected

    -- | Calculation for GetKeycode (Layer 1, Row 2, Col 3):
    -- Command Byte: 0x04
    -- Arguments: 0x01 (Layer), 0x02 (Row), 0x03 (Col)
    -- Total: [0x04, 0x01, 0x02, 0x03] + 28 bytes of padding
    it "serializes GetKeycode (0x02) and its coordinates correctly" $ do
      let cmd = GetKeycode (Layer 1) (Row 2) (Col 3)
          expected = BS.pack $ [0x04, 1, 2, 3] ++ replicate 28 0x00
      serializeViaCmd cmd `shouldBe` expected

    -- | Calculation for SetKeycode (Layer 1, Row 2, Col 3, Keycode 0x1234):
    -- Command Byte: 0x05
    -- Arguments: 0x01 (Layer), 0x02 (Row), 0x03 (Col)
    -- Keycode: 0x12 (High Byte), 0x34 (Low Byte)
    -- Total: [0x05, 0x01, 0x02, 0x03, 0x12, 0x34] + 26 bytes of padding
    it "serializes SetKeycode (0x05) with split high/low keycode bytes" $ do
      let cmd = SetKeycode (Layer 1) (Row 2) (Col 3) (Keycode 0x1234)
          expected = BS.pack $ [0x05, 1, 2, 3, 0x12, 0x34] ++ replicate 26 0x00
      serializeViaCmd cmd `shouldBe` expected

    it "serializes GetLayerCount (0x11) correctly with 31 bytes of padding" $ do
      let expected = BS.pack $ 0x11 : replicate 31 0x00
      serializeViaCmd GetLayerCount `shouldBe` expected
