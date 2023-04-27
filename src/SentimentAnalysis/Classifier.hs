module SentimentAnalysis.Classifier (
  classifySentiment,
) where

import SentimentAnalysis.Sentiment (Sentiment(..))

-- | 'classifySentiment' function takes a preprocessed text and returns the sentiment.
classifySentiment :: Text -> _ -> IO Sentiment
classifySentiment text loadModel = do
  model <- loadModel "distilbert-base-uncased-finetuned-sst-2-english"
  tokenizer <- loadTokenizer "distilbert-base-uncased-finetuned-sst-2-english"
  let tokens = tokenize tokenizer text
  sentimentClassifier model tokens