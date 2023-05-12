module ScraperSpec.Polygon where


import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 as C8 ()
import Data.Text.Lazy.Encoding qualified as LE
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.HTML.TagSoup (parseTags)

import Common (Article, convertTag)
import Data.Maybe (fromJust)
import Scraper.Polygon (extractArticlesPolygon, parsePolygonArticle)

-- | Function to perform the test.
spec :: Spec
spec = do
  describe "extractArticlesPolygon" $ do
    it "extracts the correct articles from Polygon's index page" $ do
      -- Load the sample index page HTML.
      htmlBytes <- readFileLBS "test/html/polygon_index.html"
      let html = LE.decodeUtf8 htmlBytes
      -- Extract the articles from the HTML.
      let articles = extractArticlesPolygon (toText html) -- Convert to strict Text
      -- Load the expected articles.
      expectedArticlesBytes <- readFileLBS "test/expected/polygon_index_articles.txt"
      let expectedArticles = decode expectedArticlesBytes :: Maybe [Article] -- specify the type as Maybe [Article]
      -- Check if the extracted articles match the expected articles.
      articles `shouldBe` fromJust expectedArticles -- use fromJust to extract the value from Maybe
  
  describe "parsePolygonArticle" $ do
    it "parses the correct article from Polygon's individual article page" $ do
      -- Load the sample article page HTML.
      htmlBytes <- readFileLBS "test/html/polygon_article.html"
      let html = LE.decodeUtf8 htmlBytes
      -- Parse the article from the HTML.
      let tags = map convertTag $ parseTags $ toString html
      let article = parsePolygonArticle "https://www.polygon.com/sample-article" tags
      -- Load the expected article.
      expectedArticleBytes <- readFileLBS "test/expected/polygon_article.txt"
      let expectedArticle = decode expectedArticleBytes :: Maybe Article -- specify the type as Maybe Article
      -- Check if the parsed article matches the expected article.
      article `shouldBe` expectedArticle -- Ensure the types match