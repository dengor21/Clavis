module KeycodesSpec (spec) where

import Test.Hspec
import Keycodes

spec :: Spec
spec = do

  describe "Alias translation (translateToHex)" $ do
    it "parses aliases to the actual keycodes" $ do
      translateToHex "esc" `shouldBe` Right 0x0029

    it "parses the an hex string with 0x notation" $ do
      translateToHex "0x0029" `shouldBe` Right 0x0029

    it "parses an hex string without prefix" $ do
      translateToHex "0029" `shouldBe` Right 0x0029
      
    it "sends an error message if it can't translate the input" $ do
      translateToHex "0xzzzz" `shouldBe` Left "Invalid keycode or hex string in layout: 0xzzzz" 

  describe "Hex translation to Alias (translateToString)" $ do
    it "parses a hex code and return the correct alias" $ do
      translateToString 0x00E0 `shouldBe` "lctl"

    it "returns an hex string, if an alias can't be found" $ do
      translateToString 0xFFFF `shouldBe` "0xffff"


