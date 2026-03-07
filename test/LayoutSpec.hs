module LayoutSpec (spec) where

import Test.Hspec
import Layout
import Types

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

  describe "Layout Transformation (toElectricalLayer)" $ do
    let
      -- A simple 1x2 physical keyboard
      mapping = [[(0,0), (0,1)]]
      -- User says: Key 1 is "A", Key 2 is "B"
      physicalLayer = [["KC_A", "KC_B"]]

    it "correctly maps physical keys to their electrical positions" $ do
      let result = toElectricalLayer physicalLayer mapping
      -- In this case, Row 0 Col 0 should be "KC_A", Row 0 Col 1 "KC_B"
      result `shouldBe` [["KC_A", "KC_B"]]

    it "fills gaps in the electrical matrix with 'none'" $ do
      -- Mapping says: Key 1 is at (0,0), Key 2 is at (1,1) (a diagonal jump)
      let gapMapping = [[(0,0), (1,1)]]
          gapPhysical = [["KC_ESC", "KC_ENTER"]]
          result = toElectricalLayer gapPhysical gapMapping

      -- We expect a 2x2 matrix where (0,1) and (1,0) are 'none'
      result `shouldBe` [["KC_ESC", "none"], ["none", "KC_ENTER"]]

  describe "Keycode Response Parsing (parseKeycodeResponse)" $ do
    it "decodes high and low bytes into a 16-bit keycode" $ do
      -- [0x04, layer, row, col, high, low, ...]
      let response = [0x04, 0x00, 0x00, 0x00, 0x12, 0x34] ++ replicate 26 0x00
      parseKeycodeResponse response `shouldBe` 0x1234

    it "decodes a zero keycode" $ do
      let response = [0x04, 0x00, 0x00, 0x00, 0x00, 0x00] ++ replicate 26 0x00
      parseKeycodeResponse response `shouldBe` 0x0000

    it "falls back to 0x0000 for malformed responses" $ do
      parseKeycodeResponse [] `shouldBe` 0x0000
      parseKeycodeResponse [0x01, 0x02] `shouldBe` 0x0000

  describe "Physical Layout Transformation (toPhysicalLayout)" $ do
    it "reorders electrical matrix rows into physical rows for all layers" $ do
      -- Electrical: 2x2 matrix, mapping picks keys in a different order
      let rawLayers = [[["a", "b"], ["c", "d"]]]
          mapping = [[(1,0), (0,1)], [(0,0), (1,1)]]
          result = toPhysicalLayout rawLayers mapping
      result `shouldBe` [[["c", "b"], ["a", "d"]]]

    it "handles multiple layers" $ do
      let rawLayers = [ [["a", "b"], ["c", "d"]]
                      , [["e", "f"], ["g", "h"]]
                      ]
          mapping = [[(0,0), (0,1)]]
          result = toPhysicalLayout rawLayers mapping
      result `shouldBe` [[["a", "b"]], [["e", "f"]]]

  describe "Physical Layout Single (toPhysicalLayoutSingle)" $ do
    it "reorders a single electrical layer into physical layout" $ do
      let rawLayer = [["a", "b"], ["c", "d"]]
          mapping = [[(1,1), (0,0)]]
          result = toPhysicalLayoutSingle rawLayer mapping
      result `shouldBe` [["d", "a"]]

  describe "Coordinate List Generation (generateCoordinateList)" $ do
    it "generates all (layer, row, col) coordinates for querying" $ do
      let flatMapping = [(0,0), (0,1), (1,0), (1,1)]
          result = generateCoordinateList 2 flatMapping
      result `shouldBe` [ (Layer 0, Row 0, Col 0), (Layer 0, Row 0, Col 1)
                        , (Layer 0, Row 1, Col 0), (Layer 0, Row 1, Col 1)
                        , (Layer 1, Row 0, Col 0), (Layer 1, Row 0, Col 1)
                        , (Layer 1, Row 1, Col 0), (Layer 1, Row 1, Col 1)
                        ]

    it "generates coordinates for a single layer" $ do
      let flatMapping = [(0,0), (0,1)]
          result = generateCoordinateList 1 flatMapping
      result `shouldBe` [ (Layer 0, Row 0, Col 0), (Layer 0, Row 0, Col 1) ]

  describe "Utility Functions" $ do
    it "chunksOf splits a list into groups" $ do
      chunksOf 3 [1 :: Int, 2, 3, 4, 5, 6, 7] `shouldBe` [[1,2,3], [4,5,6], [7]]

    it "chunksOf on empty list returns empty" $ do
      chunksOf 3 ([] :: [Int]) `shouldBe` []

    it "replaceAt replaces the element at the given index" $ do
      replaceAt 1 "X" ["a", "b", "c"] `shouldBe` ["a", "X", "c"]

    it "updateMatrix updates a cell in a 2D list" $ do
      let mat = [["a", "b"], ["c", "d"]]
      updateMatrix mat ((0, 1), "Z") `shouldBe` [["a", "Z"], ["c", "d"]]
