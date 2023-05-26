{-# LANGUAGE OverloadedStrings #-}

module FundamentalAnalysis.FundamentalAnayalsisScript (
  callFundamentalAnalysisScript,
) where

import Data.Aeson (Value, decode)
import System.Process (readProcess)

{- | 'callFundamentalAnalysisScript' takes a company ticker as a Text argument,
 calls the fundamental analysis script, and parses the result into a JSON Value.
-}
callFundamentalAnalysisScript :: Text -> IO (Maybe Value)
callFundamentalAnalysisScript companyTicker = do
  let tickerStr = toString companyTicker
  rawOutput <- readProcess "runghc" ["FundamentalAnalysis.hs", tickerStr] ""
  let jsonResult = decode $ fromString rawOutput :: Maybe Value
  return jsonResult