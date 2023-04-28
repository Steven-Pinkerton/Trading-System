module Test.ArticleExtractionSpec (spec) where

import Test.Hspec
import ArticleExtraction.ArticleExtraction (Article(..), extractAndPreprocess)

spec :: Spec
spec = do
  describe "ArticleExtraction.extractAndPreprocess" $ do
    it "should extract and preprocess articles from a given URL" $ do
      let testUrl = "https://example.com/test_articles.html"
      result <- extractAndPreprocess testUrl
      case result of
        Left err -> expectationFailure $ "Failed to fetch articles: " ++ show err
        Right articles -> do
          length articles `shouldBe` 3
          title (head articles) `shouldBe` "Sample Article 1"
          -- Add more assertions to check the content of the extracted articles