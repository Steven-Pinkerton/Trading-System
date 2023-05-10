{-# LANGUAGE OverloadedStrings #-}

module Scraper.GamasutraSpec where

import Data.Text.Lazy.Encoding (decodeUtf8)
import Scraper.Parsers (parseGamasutraArticle)
import Test.Hspec (Spec, describe, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseGamasutraArticle" $ do
    it "extracts the correct content from the first Gamasutra article" $ do
      article1Html <- decodeUtf8 <$> readFileLBS "test_data/gamasutra_article1.html"
      article1Content <- parseGamasutraArticle article1Html
      expectedContent <- decodeUtf8 <$> readFileLBS "expected_test_results/gamasutra_article1.expected.txt"
      article1Content `shouldBe` Just expectedContent

    it "extracts the correct content from the second Gamasutra article" $ do
      article2Html <- decodeUtf8 <$> readFileLBS "test_data/gamasutra_article2.html"
      article2Content <- parseGamasutraArticle article2Html
      expectedContent <- decodeUtf8 <$> readFileLBS "expected_test_results/gamasutra_article2.expected.txt"
      article2Content `shouldBe` Just expectedContent