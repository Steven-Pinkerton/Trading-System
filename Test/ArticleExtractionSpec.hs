module ArticleExtractionSpec where

import Test.Hspec
    ( describe, it, shouldBe, Spec, shouldSatisfy )
import Common ( Article(content, title) )
import Scraper.Parsers ( extractArticles )

spec :: Spec
spec = do
  describe "extractArticles" $ do
    it "should return a non-empty list of articles for a valid HTML input" $ do
      let html = "<html><body><div class=\"article\"><h1>Sample Article 1</h1><p>Content 1</p></div><div class=\"article\"><h1>Sample Article 2</h1><p>Content 2</p></div></body></html>"
      let articles = extractArticles html
      length articles `shouldSatisfy` (> 0)

    it "should extract the correct title and content for the first article" $ do
      let html = "<html><body><div class=\"article\"><h1>Sample Article 1</h1><p>Content 1</p></div><div class=\"article\"><h1>Sample Article 2</h1><p>Content 2</p></div></body></html>"
      let articles = extractArticles html
      (title <$> viaNonEmpty head articles) `shouldBe` Just "Sample Article 1"
      (content <$> viaNonEmpty head articles) `shouldBe` Just "Content 1"