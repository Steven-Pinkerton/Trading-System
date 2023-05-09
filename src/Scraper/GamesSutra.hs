{-# LANGUAGE OverloadedStrings #-}

module Scraper.GamesSutra (
  fetchGamasutraArticles,
) where

import Common (Article (..))
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Scraper.Parsers (parseGamasutra, parseGamasutraUrls)
import Prelude hiding (error)

-- Update the fetchArticle function
-- Now it takes a Text parameter and returns IO (Either Text Article).
fetchArticle :: Text -> IO (Either Text Article)
fetchArticle url = do
  request <- parseRequest (toString url)
  response <- httpLBS request
  let body = decodeUtf8 $ getResponseBody response
  return $ parseGamasutra url body

-- Update the fetchGamasutraArticles function
-- Now it returns IO (Either Text [Article]).
fetchGamasutraArticles :: IO (Either Text [Article])
fetchGamasutraArticles = do
  request <- parseRequest "https://www.gamasutra.com"
  response <- httpLBS request
  let body = decodeUtf8 $ getResponseBody response
  case parseGamasutraUrls body of
    Left error -> return $ Left error
    Right urls -> sequenceA <$> traverse fetchArticle urls