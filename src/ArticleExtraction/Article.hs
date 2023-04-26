module ArticleExtraction.Article (
  Article (..),
  articleToText,
  textToArticle,
) where

import Data.Text qualified as T
import Data.List ((!!))

-- | 'Article' data type represents an extracted article.
data Article = Article
  { title :: Text
  , url :: Text
  , content :: Text
  }
  deriving stock (Eq, Show)

-- | 'articleToText' function converts an 'Article' to 'Text'.
articleToText :: Article -> Text
articleToText article =
  T.intercalate
    "\n"
    [ "Title: " <> title article
    , "URL: " <> url article
    , "Content: " <> content article
    ]

{- | 'textToArticle' function converts a 'Text' to an 'Article'.
 This function should be the inverse of 'articleToText'.
 You may need to adjust this function based on the actual format used when saving articles.
-}
textToArticle :: Text -> Maybe Article
textToArticle text = do
  let lines' = lines text
  guard (length lines' >= 3)
  let titleLine = lines' !! 0
      urlLine = lines' !! 1
      contentLine = lines' !! 2
  title' <- T.stripPrefix "Title: " titleLine
  url' <- T.stripPrefix "URL: " urlLine
  content' <- T.stripPrefix "Content: " contentLine
  return $ Article title' url' content'