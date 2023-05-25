{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scraper.Requests (
  fetchPage,
  fetchPageWithRetry,
) where

import Control.Exception (SomeException, try)
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Relude
    ( ($),
      Monad(return),
      Semigroup((<>)),
      Int,
      IO,
      Either,
      isLeft,
      (.),
      ToString(toString) )

-- Constants for retrying
initialDelay :: Int
initialDelay = 1000000 -- 1 second

maxRetries :: Int
maxRetries = 5

{- | 'fetchPage' takes a URL as input and fetches the content of the web page.
 It returns the content as a ByteString.
-}
fetchPage :: Text -> IO ByteString
fetchPage url = do
  req <- parseRequest (toString url)
  res <- httpBS req
  return $ getResponseBody res

{- | 'fetchPageWithRetry' takes a URL as input and fetches the content of the
 web page with retrying in case of failures. It uses an exponential backoff
 strategy with a limited number of retries.
-}
fetchPageWithRetry :: Text -> IO (Either SomeException ByteString)
fetchPageWithRetry url = retrying retryPolicy shouldRetry $ \_ -> try $ fetchPage url
  where
    retryPolicy = exponentialBackoff initialDelay <> limitRetries maxRetries
    shouldRetry _ = return . isLeft