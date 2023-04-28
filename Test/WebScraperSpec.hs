module Test.WebScraperSpec (Spec) where

-- Import your webScraper module here
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import WebScraper

spec :: Spec
spec = do
  describe "scrapeArticles" $ do
    it "should return a non-empty list of articles for a valid URL" $ do
      manager <- newManager tlsManagerSettings
      articles <- scrapeArticles "https://example.com" manager
      length articles `shouldSatisfy` (> 0)