module Scraper.GamesSutra (
  fetchGamasutraArticles,
) where

import ArticleExtraction.Article (Article)
import Data.Text (Text)
import Data.Text qualified as T
import Scraper.Parsers (parseGamasutraArticles)
import Scraper.Requests (fetchHtml)

-- URL of Gamasutra's news page
gamasutraUrl :: Text
gamasutraUrl = "https://www.gamasutra.com/news"

-- | Fetch the latest Gamasutra articles
fetchGamasutraArticles :: IO [Article]
fetchGamasutraArticles = do
  html <- fetchHtml gamasutraUrl
  case parseGamasutraArticles html of
    Left err -> do
      putStrLn $ "Error parsing Gamasutra articles: " <> T.unpack err
      return []
    Right articles -> return articles