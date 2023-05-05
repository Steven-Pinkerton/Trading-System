module ArticleExtraction (
  Article (..),
  extractAndPreprocess,
  preprocessArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.Text (Text, isInfixOf, pack)
import Network.HTTP.Client
    ( HttpException(HttpExceptionRequest),
      Manager,
      Request(host),
      HttpExceptionContent(StatusCodeException),
      defaultRequest,
      host,
      responseHeaders )
import Scraper.Parsers (extractArticles)
import Scraper.Requests ( fetchPage, fetchPageWithRetry )
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.ByteString.Char8 (pack)

-- | 'extractAndPreprocess' function takes a URL and returns a list of preprocessed articles.
extractAndPreprocess :: String -> IO (Either HttpException [Article])
extractAndPreprocess url' = do
  result <- fetchPageWithRetry (pack url')
  case result of
    Left someErr ->
      case fromException someErr of
        Just httpErr -> return $ Left httpErr
        Nothing -> return $ Left (toHttpException someErr)
    Right content' -> do
      let decodedContent = decodeUtf8 content'
          articles = extractArticlesForSite (pack url') decodedContent
          preprocessedArticles = map preprocessArticle articles
      return $ Right preprocessedArticles

toHttpException :: SomeException -> HttpException
toHttpException someErr = HttpExceptionRequest defaultRequest (StatusCodeException status headers)
  where
    defaultRequest = defaultRequest {host = "example.com"}
    status = Status 999 "Unknown Error"
    headers :: ResponseHeaders
    headers = [(pack "X-Error-Message", pack (displayException someErr))]

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