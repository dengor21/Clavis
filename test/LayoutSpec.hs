module LayoutSpec (spec) where

import Test.Hspec
import Layout

spec :: Spec
spec = do

  describe "Hex Parsing (parseHex)" $ do
    it "parses hex strings with 0x prefix" $ do
      parseHex "0x1234" `shouldBe` Right 0x1234

    it "parses hex strings without 0x prefix" $ do
      parseHex "ABCD" `shouldBe` Right 0xABCD

    it "handles lowercase hex strings" $ do
      parseHex "0xcafe" `shouldBe` Right 0xCAFE

    it "sends an error on a wrong input" $ do
      parseHex "0xzzzz" `shouldBe` Left "Could not parse hex string: 0xzzzz"

  describe "Coordinate Extraction (parseCoordinate)" $ do
    it "extracts (row, col) from a standard VIA string" $ do
      parseCoordinate "0,1\nEsc" `shouldBe` Just (0, 1)

    it "handles strings with spaces and no label" $ do
      parseCoordinate " 2 , 3 " `shouldBe` Just (2, 3)

    it "returns Nothing for non-coordinate strings" $ do
      parseCoordinate "JustALabel" `shouldBe` Nothing

  describe "Layout Transformation (toElectricalLayout)" $ do
    let 
      -- A simple 1x2 physical keyboard
      mapping = [[(0,0), (0,1)]]
      -- User says: Key 1 is "A", Key 2 is "B"
      physicalLayers = [[["KC_A", "KC_B"]]]
      
    it "correctly maps physical keys to their electrical positions" $ do
      let result = toElectricalLayout physicalLayers mapping
      -- In this case, Row 0 Col 0 should be "KC_A", Row 0 Col 1 "KC_B"
      result `shouldBe` [[["KC_A", "KC_B"]]]

    it "fills gaps in the electrical matrix with 'none'" $ do
      -- Mapping says: Key 1 is at (0,0), Key 2 is at (1,1) (a diagonal jump)
      let gapMapping = [[(0,0), (1,1)]]
          gapPhysical = [[["KC_ESC", "KC_ENTER"]]]
          result = toElectricalLayout gapPhysical gapMapping
      
      -- We expect a 2x2 matrix where (0,1) and (1,0) are 'none'
      result `shouldBe` [[["KC_ESC", "none"], ["none", "KC_ENTER"]]]
