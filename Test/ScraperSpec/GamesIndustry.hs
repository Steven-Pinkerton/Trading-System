{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec.GamesIndustry where

import Data.Aeson (decode)
import Data.Text.Lazy.Encoding qualified as TLE
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Scraper.GamesIndustry (fetchGamesIndustyArticleContent)



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "extractContent" $ do
    it "extracts the correct content from the Mario article" $ do
      marioUrlLBS <- readFileLBS "test_data/mario.url"
      let marioUrl = toString . TLE.decodeUtf8 $ marioUrlLBS
      marioContent <- fetchGamesIndustyArticleContent marioUrl
      expectedContentJson <- readFileLBS "expected_test_results/mario.expected.json"
      let expectedContent = decode expectedContentJson
      marioContent `shouldBe` expectedContent

    it "extracts the correct content from the Meta article" $ do
      metaUrlLBS <- readFileLBS "test_data/meta.url"
      let metaUrl = toString . TLE.decodeUtf8 $ metaUrlLBS
      metaContent <- fetchGamesIndustyArticleContent metaUrl
      expectedContentJson <- readFileLBS "expected_test_results/meta.expected.json"
      let expectedContent = decode expectedContentJson
      metaContent `shouldBe` expectedContent