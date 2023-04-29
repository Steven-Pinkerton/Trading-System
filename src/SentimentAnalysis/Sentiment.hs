{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use toText" #-}
module SentimentAnalysis.Sentiment (
  Sentiment (..),
  sentimentFromScore,
  sentimentToText,
) where

import Data.Text qualified as T

data Sentiment = Positive | Neutral | Negative
  deriving stock (Eq, Show, Ord, Enum)

-- | 'sentimentFromScore' function takes a sentiment score and returns a 'Sentiment' based on the provided threshold values.
sentimentFromScore :: Double -> Double -> Double -> Sentiment
sentimentFromScore thresholdPositive thresholdNegative score
  | score >= thresholdPositive = Positive
  | score <= thresholdNegative = Negative
  | otherwise = Neutral

-- | 'sentimentToText' function takes a 'Sentiment' and returns its textual representation.
sentimentToText :: Sentiment -> Text
sentimentToText sentiment = case sentiment of
  Positive -> T.pack "Positive"
  Neutral -> T.pack "Neutral"
  Negative -> T.pack "Negative"