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
      marioHtml <- readFileLBS "test_data/mario.html"
      marioContent <- fetchArticleContent (decodeUtf8 marioHtml)
      marioContent `shouldBe` Just "This is the expected content from the Mario article."

    it "extracts the correct content from the Meta article" $ do
      metaHtml <- readFileLBS "test_data/meta.html"
      metaContent <- fetchArticleContent (decodeUtf8 metaHtml)
      metaContent `shouldBe` Just "This is the expected content from the Meta article."