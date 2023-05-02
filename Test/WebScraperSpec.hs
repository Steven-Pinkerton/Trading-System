module WebScraperSpec where

-- Import your webScraper module here
import Test.Hspec ( describe, it, shouldSatisfy, Spec )
import Scraper.Scraper (fetchPage, extractLinks)
import qualified Data.ByteString as B

spec :: Spec
spec = do
  describe "fetchPage" $ do
    it "should return a non-empty ByteString for a valid URL" $ do
      page <- fetchPage "https://www.bbc.co.uk/news/business-65123115"
      page `shouldSatisfy` (not . B.null)

  describe "extractLinks" $ do
    it "should return a non-empty list of links for a valid HTML input" $ do
      let html = "<html><body><a href=\"https://example.com/article1\">Article 1</a><a href=\"https://example.com/article2\">Article 2</a></body></html>"
      let links = extractLinks html
      length links `shouldSatisfy` (> 0)