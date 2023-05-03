{-# LANGUAGE OverloadedStrings #-}

module Scraper.ParsersSpec where

import Test.Hspec
import Scraper.Parsers ( extractArticlesGamesIndustry )
import Common ( Article(..))

spec :: Spec
spec = do
  describe "extractArticlesGamesIndustry" $ do
    it "extracts articles from sample GamesIndustry HTML" $ do
      let sampleHtml = "<!-- Sample HTML with the structure of GamesIndustry site -->"
      let expectedArticles =
            [ Article
                { title = "Sample Title"
                , url = "https://www.example.com/sample-article"
                , content = "Sample content of the article."
                }
            ]
      extractArticlesGamesIndustry sampleHtml `shouldBe` expectedArticles