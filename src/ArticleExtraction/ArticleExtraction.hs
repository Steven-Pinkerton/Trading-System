module ArticleExtraction (
  Article (..),
  extractAndPreprocess,
  preprocessArticle,
  scrapeArticles,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.CaseInsensitive (mk)
import Data.Text (isInfixOf)
import Network.HTTP.Client (
  HttpException (HttpExceptionRequest),
  HttpExceptionContent (StatusCodeException),
  Manager,
  Request (host),
  Response (responseBody, responseCookieJar, responseVersion),
  host,
  responseHeaders,
  responseStatus,
 )
import Network.HTTP.Types (Status (..), http11)
import Scraper.GamesIndustry (extractArticlesGamesIndustry)
import Scraper.Parsers (extractArticles)
import Scraper.Requests (fetchPage, fetchPageWithRetry)

-- | 'extractAndPreprocess' function takes a URL and returns a list of preprocessed articles.
extractAndPreprocess :: String -> IO (Either HttpException [Article])
extractAndPreprocess url' = do
  result <- fetchPageWithRetry (toText url')
  case result of
    Left someErr ->
      case fromException someErr of
        Just httpErr -> return $ Left httpErr
        Nothing -> return $ Left (toHttpException someErr)
    Right content' -> do
      let decodedContent = decodeUtf8 content'
          articles = extractArticlesForSite (toText url') decodedContent
          preprocessedArticles = map preprocessArticle articles
      return $ Right preprocessedArticles

toHttpException :: SomeException -> HttpException
toHttpException someErr = HttpExceptionRequest defaultRequest (StatusCodeException response ())
  where
    defaultRequest = defaultRequest {host = "example.com"}
    response :: Response ()
    response =
      response
        { responseStatus = Status 999 "Unknown Error"
        , responseVersion = http11
        , responseHeaders = [(mk (encodeUtf8 "X-Error-Message"), encodeUtf8 (displayException someErr))]
        , responseBody = ()
        , responseCookieJar = mempty
        }

-- | 'preprocessArticle' function takes an 'Article' and returns a preprocessed 'Article'.
preprocessArticle :: Article -> Article
preprocessArticle (MkArticle title' url' content') =
  let preprocessedContent = preprocess content'
   in MkArticle title' url' (unwords preprocessedContent)

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
  result <- fetchPageWithRetry url
  case result of
    Left _ -> return [] -- You can decide how to handle the error case here
    Right html -> do
      let articles = extractArticlesForSite url (decodeUtf8 html)
      return articles