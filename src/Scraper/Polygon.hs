module Scraper.Polygon (
  extractArticlesPolygon,
parsePolygonArticle) where

import Text.XML.Cursor qualified as Cursor

import Common (Article (..))
import Text.HTML.TagSoup
    ( sections, innerText, isTagOpenName, Tag )
import Text.XML (def, parseLBS)
import Text.XML.Cursor (fromDocument, ($//), (&/))

extractArticlesPolygon :: Text -> [Article]
extractArticlesPolygon html = do
  let doc = case Text.XML.parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  let cursor = fromDocument doc
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "c-entry-content"
  mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
        let titleNodes = articleNode $// Cursor.element "title" &/ Cursor.content
        let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
        let contentText = unwords contentNodes
        
        case viaNonEmpty head titleNodes of
            Just titleText -> Just $ Article titleText "" contentText -- Assuming "" as URL for now
            Nothing -> Nothing

parsePolygonArticle :: Text -> [Tag Text] -> Maybe Article
parsePolygonArticle url' tags = do
  titleTags <- viaNonEmpty head (sections (isTagOpenName "title") tags)
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)

  titleTag <- viaNonEmpty head titleTags
  contentTag <- viaNonEmpty head contentTags

  let titleText = innerText [titleTag]
  let contentText = innerText [contentTag]

  return $ Article titleText url' contentText