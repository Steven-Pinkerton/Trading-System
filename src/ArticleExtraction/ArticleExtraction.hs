module ArticleExtraction (
  Article (..),
  extractAndPreprocess,
  fetchUrl,
  preprocessArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Control.Exception (catch)
import Data.Text (Text, isInfixOf, pack)
import Network.HTTP.Client (HttpException, parseRequest, Manager)
import Network.HTTP.Simple (getResponseBody, httpBS)
import Scraper.Parsers (extractArticles)
import Scaraper.Requests

-- | 'extractAndPreprocess' function takes a URL and returns a list of preprocessed articles.
extractAndPreprocess :: String -> IO (Either HttpException [Article])
extractAndPreprocess url' = do
  result <- fetchUrl url'
  case result of
    Left err -> return $ Left err
    Right content' -> do
      let articles = extractArticlesForSite (pack url') content'
          preprocessedArticles = map preprocessArticle articles
      return $ Right preprocessedArticles

-- | 'fetchUrl' function takes a URL and returns the content as 'ByteString' or an 'HttpException'.
fetchUrl :: String -> IO (Either HttpException ByteString)
fetchUrl url' = do
  req <- parseRequest url'
  (Right . getResponseBody <$> httpBS req) `catch` (return . Left)

-- | 'preprocessArticle' function takes an 'Article' and returns a preprocessed 'Article'.
preprocessArticle :: Article -> Article
preprocessArticle (Article title' url' content') =
  let preprocessedContent = preprocess content'
   in Article title' url' (unwords preprocessedContent)

data Site = GamesIndustryBiz | OtherSite

identifySite :: Text -> Site
identifySite url =
  if "gamesindustry.biz" `isInfixOf` url
    then GamesIndustryBiz
    else OtherSite

extractArticlesForSite :: Text -> Text -> [Article]
extractArticlesForSite siteUrl html =
  case identifySite siteUrl of
    GamesIndustryBiz -> extractArticlesGamesIndustry html
    OtherSite -> extractArticles html

scrapeArticles :: Text -> Manager -> IO [Article]
scrapeArticles url manager = do
  html <- fetchPage url manager
  let articles = extractArticlesForSite url html
  return articles