{-# LANGUAGE OverloadedStrings #-}

module Scraper.ParsersSpec where

import Data.Text.Lazy.Encoding ()
import Scraper.Parsers (fetchArticleContent)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "extractContent" $ do
    it "extracts the correct content from the Mario article" $ do
      marioHtml <- decodeUtf8 <$> readFileLBS "test_data/mario.html"
      marioContent <- fetchArticleContent marioHtml
      expectedContent <- decodeUtf8 <$> readFileLBS "expected_test_results/mario.expected.txt"
      marioContent `shouldBe` Just expectedContent

    it "extracts the correct content from the Meta article" $ do
      metaHtml <- decodeUtf8 <$> readFileLBS "test_data/meta.html"
      metaContent <- fetchArticleContent metaHtml
      expectedContent <- decodeUtf8 <$> readFileLBS "expected_test_results/meta.expected.txt"
      metaContent `shouldBe` Just expectedContent