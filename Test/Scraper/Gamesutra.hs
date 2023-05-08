{-# LANGUAGE OverloadedStrings #-}

module Scraper.GamasutraSpec where

import Common ()
import Scraper.Parsers ()
import Test.Hspec ( describe, it, pending, Spec )
import Text.HTML.TagSoup (parseTags)

spec :: Spec
spec = do
  describe "Gamasutra article parsing" $ do
    it "should parse a single Gamasutra article" $ do
      sampleHtml <- readFile "test_data/gamasutra_sample.html"
      expectedOutput <- readFile "test_data/gamasutra_expected_output.txt"
      let tags = parseTags (pack sampleHtml)
      let expectedArticle = read (unpack expectedOutput) :: Article
      parseGamasutraArticle tags `shouldBe` Just expectedArticle

    it "should extract a list of Gamasutra articles" $ do
      -- Add test case for a list of articles
      pending