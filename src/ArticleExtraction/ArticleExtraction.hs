module ArticleExtraction.Article (
  Article (..),
   extractAndPreprocess,
  fetchUrl,
  preprocessArticle
) where

import Network.HTTP.Client (HttpException, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpBS)
import ArticleExtraction.Preprocessing (preprocess)
import Scraper.Parsers (extractArticles)
import Control.Exception (catch)

data Article = Article
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show)

-- | 'extractAndPreprocess' function takes a URL and returns a list of preprocessed articles.
extractAndPreprocess :: String -> IO (Either HttpException [Article])
extractAndPreprocess url' = do
  result <- fetchUrl url'
  case result of
    Left err -> return $ Left err
    Right content' -> do
      let articles = extractArticles content'
          preprocessedArticles = map preprocessArticle articles
      return $ Right preprocessedArticles

-- | 'fetchUrl' function takes a URL and returns the content as 'ByteString' or an 'HttpException'.
fetchUrl :: String -> IO (Either HttpException ByteString)
fetchUrl url = do
  req <- parseRequest url
  (Right . getResponseBody <$> httpBS req) `catch` (return . Left)

-- | 'preprocessArticle' function takes an 'Article' and returns a preprocessed 'Article'.
preprocessArticle :: Article -> Article
preprocessArticle (Article title' url' content') =
  let preprocessedContent = preprocess content'
   in Article title' url' (unwords preprocessedContent)