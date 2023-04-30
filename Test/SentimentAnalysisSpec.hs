module SentimentAnalysisSpec where

import SentimentAnalysis.Classifier (classifySentiment)
import SentimentAnalysis.Sentiment (Sentiment (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "classifySentiment" $ do
    it "should classify a positive sentiment correctly" $ do
      sentiment <- classifySentiment "I love this product! It's amazing."
      sentiment `shouldBe` Positive

    it "should classify a negative sentiment correctly" $ do
      sentiment <- classifySentiment "I hate this product! It's terrible."
      sentiment `shouldBe` Negative

    it "should classify a neutral sentiment correctly" $ do
      sentiment <- classifySentiment "This is just an ordinary statement."
      sentiment `shouldBe` Neutral