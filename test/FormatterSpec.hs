module FormatterSpec (spec) where

import Test.Hspec
import Formatter
import Types
import Data.Aeson (eitherDecode)

spec :: Spec
spec = do

  describe "Layout Formatting (formatLayoutMatrix)" $ do
    it "produces valid JSON that can be parsed back into a UserConfig" $ do
      let uc = UserConfig 1 "Test Keyboard" "0x1234" "0x5678" []
                [ LayerData 0 [["esc", "1", "2"], ["tab", "q", "w"]] ]
          json = formatLayoutMatrix uc
      case eitherDecode json :: Either String UserConfig of
        Left err -> expectationFailure $ "Failed to parse formatted JSON: " ++ err
        Right parsed -> do
          formatVersion parsed `shouldBe` 1
          keyboardName parsed `shouldBe` "Test Keyboard"
          vid parsed `shouldBe` "0x1234"
          pid parsed `shouldBe` "0x5678"
          length (layers parsed) `shouldBe` 1

    it "preserves layer data through a format round-trip" $ do
      let originalKeys = [["a", "b", "c"], ["d", "e", "f"]]
          uc = UserConfig 1 "RoundTrip" "0xAAAA" "0xBBBB" []
                [ LayerData 0 originalKeys ]
          json = formatLayoutMatrix uc
      case eitherDecode json :: Either String UserConfig of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right parsed -> do
          let (parsedLayer:_) = layers parsed
          layer parsedLayer `shouldBe` 0
          keys parsedLayer `shouldBe` originalKeys

    it "formats multiple layers correctly" $ do
      let uc = UserConfig 1 "MultiLayer" "0x0001" "0x0002" []
                [ LayerData 0 [["esc", "1"]]
                , LayerData 1 [["f1", "f2"]]
                ]
          json = formatLayoutMatrix uc
      case eitherDecode json :: Either String UserConfig of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right parsed -> do
          length (layers parsed) `shouldBe` 2
          keys (layers parsed !! 0) `shouldBe` [["esc", "1"]]
          keys (layers parsed !! 1) `shouldBe` [["f1", "f2"]]
