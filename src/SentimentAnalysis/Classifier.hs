module SentimentAnalysis.Classifier (
  classifySentiment,
) where

import SentimentAnalysis.Sentiment (Sentiment(..))
import System.Process ( readProcess )
import Data.Text ( strip )


-- | 'classifySentiment' function takes a preprocessed text and returns the sentiment.
classifySentiment :: Text -> IO Sentiment
classifySentiment text = do
  let pythonScriptPath = "path/to/your/sentiment_analysis.py"
  output <- readProcess "python" [pythonScriptPath, toString text] ""
  return $ case strip (toText output) of
    "positive" -> Positive
    "negative" -> Negative
    _ -> Neutral