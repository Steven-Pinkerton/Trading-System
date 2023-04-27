module SentimentAnalysis.Classifier (
  classifySentiment,
) where

import Data.Text (Text)
import Data.Text qualified as T
import SentimentAnalysis.Sentiment (Sentiment(..))
import HuggingFace.Transformers (loadModel, loadTokenizer, tokenize, Transformer(..), sentimentClassifier)

-- | 'classifySentiment' function takes a preprocessed text and returns the sentiment.
classifySentiment :: Text -> IO Sentiment
classifySentiment text = do
  model <- loadModel "distilbert-base-uncased-finetuned-sst-2-english"
  tokenizer <- loadTokenizer "distilbert-base-uncased-finetuned-sst-2-english"
  let tokens = tokenize tokenizer text
  sentiment <- sentimentClassifier model tokens
  return sentiment