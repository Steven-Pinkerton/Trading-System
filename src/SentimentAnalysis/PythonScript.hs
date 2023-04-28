module SentimentAnalysis.PythonScript (
  callPythonScript,
  parseSentimentOutput,
  -- other utility functions for working with the Python script
) where

import System.Process (readProcess)
import SentimentAnalysis.Sentiment (Sentiment(..))
import Data.Text (strip)

-- | 'callPythonScript' function takes an 'Article' or 'Text' and returns the raw output from the Python script.
callPythonScript :: Text -> IO Text
callPythonScript input = do
  let pythonScriptPath = "path/to/your/python/script.py"
  rawOutput <- readProcess "python3" [pythonScriptPath, toString input] ""
  return (toText rawOutput)

parseSentimentOutput :: Text -> Sentiment
parseSentimentOutput rawOutput =
  case strip rawOutput of
    "positive" -> Positive
    "negative" -> Negative
    _ -> Neutral
