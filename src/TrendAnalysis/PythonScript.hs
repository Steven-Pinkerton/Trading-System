{-# LANGUAGE OverloadedStrings #-}

module TrendAnalysis.PythonScript (
  callPythonTrendScript,
  parseTrendingOutput,
) where

import System.Process (readProcess)

callPythonTrendScript :: Text -> Text -> Text -> IO Text
callPythonTrendScript url content sentiment = do
  -- Convert the Text to String for use with readProcess
  let urlStr = toString url
  let contentStr = toString content
  let sentimentStr = toString sentiment -- convert sentiment to String
  -- Call the Python script here, providing the URL, content and sentiment as arguments
  -- The exact command will depend on how your Python script is set up to take arguments
  rawOutput <- readProcess "python3" ["path/to/trend_analysis.py", urlStr, contentStr, sentimentStr] "" -- pass sentimentStr here
  return $ toText rawOutput

parseTrendingOutput :: Text -> [Text]
parseTrendingOutput rawOutput =
  -- Here, implement your logic to parse the raw output from the Python script into a list of trending topics.
  -- This will depend on how the Python script returns its results.
  -- For example, if the Python script prints one trending topic per line, you could use:
  lines rawOutput