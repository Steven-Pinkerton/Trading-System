{-# LANGUAGE OverloadedStrings #-}

module Scraper.GamasutraSpec where

import Scraper.GamesSutra (fetchGamasutraArticles)
import Test.Hspec (Spec, describe, it, shouldBe)
import Common ( Article(content) )

spec :: Spec
spec = do
  describe "Gamasutra article fetching and parsing" $ do
    -- Test case to fetch and parse Gamasutra articles
    it "should fetch and parse Gamasutra articles" $ do
      -- Fetch Gamasutra articles
      eitherArticles <- fetchGamasutraArticles

      -- Handle the result of fetching articles
      case eitherArticles of
        -- If there's an error, print the error message
        Left err -> putStrLn $ "Error: " <> toString err
        -- If articles are fetched successfully, check that the list is not empty and that each article has non-empty title, URL, and content
        Right articles -> do
          -- Check if the fetched articles list is not empty
          Prelude.null articles `shouldBe` False

          -- Read the expected content from the file
          expectedContentLBS <- readFileLBS "test_data/expected_test_results/gamesutra_expected_results.txt"

          -- Convert the expected content to Text
          let expectedContent = decodeUtf8 expectedContentLBS

          -- Find the first article with the same content as the expected content
          let matchingArticle = find (\article -> content article == expectedContent) articles

          -- Check if the matching article is found
          isJust matchingArticle `shouldBe` True