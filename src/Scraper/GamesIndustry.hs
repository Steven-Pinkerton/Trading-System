module Scraper.GamesIndustry (
  extractArticlesGamesIndustry,
) where

import qualified Text.XML.Cursor as Cursor
import ArticleExtraction.Article ( Article(Article) )
import Text.XML ( parseLBS, def )
import Text.XML.Cursor (fromDocument, ($//), (&/))

{- | 'extractArticlesGamesIndustry' takes a Text containing an HTML document and extracts a list of 'Article's from it.
It uses 'extractArticleFromNode' internally to parse individual articles.
-}
extractArticlesGamesIndustry :: Text -> [Article]
extractArticlesGamesIndustry html = do
  let doc = case Text.XML.parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  let cursor = fromDocument doc
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "summary"
  mapMaybe extractArticleFromNode articleNodes
  where
    -- \| 'extractArticleFromNode' is a local helper function for 'extractArticlesGamesIndustry'.
    --     It takes an 'articleNode' as input, which is a cursor pointing to an individual article
    --     in the HTML structure. The function extracts the title, URL, and content from the node
    --     and returns a 'Maybe Article'. If the title and URL can be extracted successfully, it
    --     returns 'Just Article'; otherwise, it returns 'Nothing'.
    --
    extractArticleFromNode articleNode = do
      let titleNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "title")
      let urlNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "href")
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (titleNodeMaybe, urlNodeMaybe) of
        (Just titleNode, Just urlNode) -> Just $ Article titleNode urlNode contentText
        _ -> Nothing