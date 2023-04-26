module ArticleExtraction.IO (
  saveArticles,
  loadArticles,
) where

import ArticleExtraction.Article (Article, articleToText, textToArticle)
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)

-- | 'saveArticles' function takes a file path and a list of 'Article's and saves them to the specified file.
saveArticles :: FilePath -> [Article] -> IO ()
saveArticles filePath articles = do
  let content = map articleToText articles
  writeFileText filePath (unlines content)

-- | 'loadArticles' function takes a file path and returns a list of 'Article's read from the specified file.
loadArticles :: FilePath -> IO [Article]
loadArticles filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- TIO.readFile filePath
      let articles = map textToArticle (lines content)
      return (catMaybes articles)
    else return []