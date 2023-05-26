{-# LANGUAGE OverloadedStrings #-}

module FundamentalAnalysis.FundamentalAnalysis.Types (
  FundamentalData (..),
) where


data FundamentalData = FundamentalData
  { companyName :: Text
  , companyTicker :: Text
  , marketCapitalization :: Maybe Double -- in millions
  , earningsPerShare :: Maybe Double
  , priceToEarningsRatio :: Maybe Double
  , dividendYield :: Maybe Double
  , debtToEquityRatio :: Maybe Double
  }
  deriving stock (Show, Eq)