module ScraperSpec.RPS where

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 as C8 ()
import Data.Text.Lazy.Encoding qualified as LE
import Test.Hspec (Spec, describe, it, shouldBe)

import Common (Article)
import Data.Maybe (fromJust)
import Scraper.RPS (extractArticlesRPS, parseRPSArticle)
import Text.HTML.DOM qualified as HTML_DOM
import Text.XML.Cursor (fromDocument)

-- | Function to perform the test.
spec :: Spec
spec = do
  describe "extractArticlesRPS" $ do
    it "extracts the correct articles from RPS's index page" $ do
      -- Load the sample index page HTML.
      htmlBytes <- readFileLBS "test_data/index pages/rps_index.html"
      let html = LE.decodeUtf8 htmlBytes
      -- Extract the articles from the HTML.
      let articles = extractArticlesRPS (toText html) -- Convert to strict Text
      -- Load the expected articles.
      expectedArticlesBytes <- readFileLBS "expected_test_results/indexes/rps.index.json"
      let expectedArticles = decode expectedArticlesBytes :: Maybe [Article] -- specify the type as Maybe [Article]
      -- Check if the extracted articles match the expected articles.
      articles `shouldBe` fromJust expectedArticles -- use fromJust to extract the value from Maybe
  
  describe "parseRPSArticle" $ do
    it "parses the correct article from RPS's individual article page" $ do
      -- Load the sample article page HTML.
      htmlBytes <- readFileLBS "test_data/despelote.html"
      -- Parse the article from the HTML.
      let cursor = fromDocument $ HTML_DOM.parseLBS htmlBytes
      let article = parseRPSArticle "https://www.rockpapershotgun.com/despelote-is-a-seriously-nostalgic-indie-taking-us-back-to-2001s-ecuador" cursor
      -- Load the expected article.
      expectedArticleBytes <- readFileLBS "expected_test_results/despelote.json"
      let expectedArticle = decode expectedArticleBytes :: Maybe Article -- specify the type as Maybe Article
      -- Check if the parsed article matches the expected article.
      article `shouldBe` expectedArticle -- Ensure the types match