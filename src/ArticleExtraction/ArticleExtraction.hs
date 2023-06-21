module ArticleExtraction (
  Article (..),
  extractAndPreprocess,
  preprocessArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.CaseInsensitive (mk)
import Data.Text (isInfixOf)
import Network.HTTP.Client (
  HttpException (HttpExceptionRequest),
  HttpExceptionContent (StatusCodeException),
  Request (host),
  Response (responseBody, responseCookieJar, responseVersion),
  host,
  responseHeaders,
  responseStatus,
 )
import Network.HTTP.Types (Status (..), http11)
import Scraper.GamesIndustry (extractArticlesGamesIndustry)
import Scraper.Parsers (extractArticles)
import Scraper.Requests (fetchPageWithRetry)
import Data.ByteString qualified as ByteString


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
      articles <- extractArticlesForSite (toText url') content'
      let preprocessedArticles = fmap preprocessArticle articles
      return $ Right preprocessedArticles

toHttpException :: SomeException -> HttpException
toHttpException someErr = HttpExceptionRequest defaultRequest (StatusCodeException response ByteString.empty)
  where
    defaultRequest = defaultRequest {host = "example.com"}
    response :: Response ()
    response =
      response
        { responseStatus = Status 999 "Unknown Error"
        , responseVersion = http11
        , responseHeaders = [(mk (encodeUtf8 ("X-Error-Message" :: Text)), encodeUtf8 (displayException someErr))]
        , responseBody = ()
        , responseCookieJar = mempty
        }
-- | 'preprocessArticle' function takes an 'Article' and returns a preprocessed 'Article'.
preprocessArticle :: Article -> Article
preprocessArticle (Article title' url' content') =
  let preprocessedContent = preprocess content'
   in Article title' url' (unwords preprocessedContent)

data Site = GamesIndustryBiz | OtherSite

identifySite :: Text -> Site
identifySite urlText =
  if "gamesindustry.biz" `isInfixOf` urlText
    then GamesIndustryBiz
    else OtherSite

extractArticlesForSite :: Text -> ByteString -> IO [Article]
extractArticlesForSite siteUrl html =
  let decodedHtml = decodeUtf8 html
   in case identifySite siteUrl of
        GamesIndustryBiz ->
          whenRight [] (extractArticlesGamesIndustry decodedHtml) return
        OtherSite -> return $ extractArticles html