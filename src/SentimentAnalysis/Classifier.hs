module SentimentAnalysis.Classifier (
  classifySentiment,
) where

import Data.Text (strip)
import SentimentAnalysis.Sentiment (Sentiment (..))
import System.Process (readProcess)

{- | 'classifySentiment' function takes a preprocessed text,
 | calls the Python script and parses the output into a 'Sentiment'.
-}
classifySentiment :: Text -> IO Sentiment
classifySentiment text = do
  let pythonScriptPath = "path/to/your/sentiment_analysis.py"
  rawOutput <- readProcess "python" [pythonScriptPath, toString text] ""
  return $ parseSentimentOutput (toText rawOutput)

parseSentimentOutput :: Text -> Sentiment
parseSentimentOutput rawOutput =
  case strip rawOutput of
    "positive" -> Positive
    "negative" -> Negative
    _ -> Neutral