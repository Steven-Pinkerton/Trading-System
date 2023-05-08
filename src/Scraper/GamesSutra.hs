module Scraper.GamesSutra (
  fetchGamasutraArticles,
) where

import Common (Article (..))
import Control.Monad (mapM)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Simple
import Scraper.Parsers (parseGamasutra)

fetchArticle :: String -> IO (Either String Article)
fetchArticle url = do
  request <- parseRequest url
  response <- httpLBS request
  let body = decodeUtf8 $ getResponseBody response
  return $ parseGamasutra url body

fetchGamasutraArticles :: IO (Either String [Article])
fetchGamasutraArticles = do
  request <- parseRequest "https://www.gamasutra.com"
  response <- httpLBS request
  let body = decodeUtf8 $ getResponseBody response
  case parseGamasutraUrls body of
    Left error -> return $ Left error
    Right urls -> mapM fetchArticle urls