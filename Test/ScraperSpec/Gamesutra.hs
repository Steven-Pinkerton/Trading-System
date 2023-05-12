{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec.Gamesutra where

import Common (Article (..))
import Data.Text.Lazy.Encoding qualified as LE
import Scraper.GamesSutra (parseGamasutraArticle)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Text.HTML.TagSoup (parseTags)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseGamasutraArticle" $ do
    it "extracts the correct content from the first Gamasutra article" $ do
      article1Html <- toText . LE.decodeUtf8 <$> readFileLBS "test_data/pokemon.html"
      let tags = parseTags article1Html
      let article1Content = content <$> parseGamasutraArticle "https://www.gamedeveloper.com/business/pok-mon-dev-game-freak-teams-with-private-division-on-new-game" tags

      expectedContent <- toText . LE.decodeUtf8 <$> readFileLBS "....."
      article1Content `shouldBe` Just expectedContent

    it "extracts the correct content from the second Gamasutra article" $ do
      article2Html <- toText . LE.decodeUtf8 <$> readFileLBS "test_data/monster.html"
      let tags = parseTags article2Html
      let article2Content = content <$> parseGamasutraArticle "https://www.gamedeveloper.com/business/monster-hunter-and-resident-evil-help-capcom-to-record-breaking-year" tags
      expectedContent <- toText . LE.decodeUtf8 <$> readFileLBS "..."
      article2Content `shouldBe` Just expectedContent
