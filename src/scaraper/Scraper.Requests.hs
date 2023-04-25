{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scaraper.Scraper.Requests
    ( fetchPage
    , fetchPageWithRetry
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Control.Retry (retrying, exponentialBackoff, limitRetries)
import Control.Exception (SomeException, try)
import Relude
    ( IO
    , Either
    , isLeft
    , toString, return, ($), (<>), (.), 
    )


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
    retryPolicy = exponentialBackoff 1000000 <> limitRetries 5 -- 1 second initial delay, 5 retries max
    shouldRetry _ = return . isLeft