module Scraper.Polygon (
  extractArticlesPolygon,
  parsePolygonArticle,
) where

import Text.XML.Cursor qualified as Cursor

import Common (Article (..))
import Text.HTML.TagSoup (
  Tag,
  innerText,
  isTagOpenName,
  sections,
 )
import Text.XML (def, parseLBS)
import Text.XML.Cursor (fromDocument, ($//), (&/))

-- | Extracts articles from Polygon's index page HTML text.
extractArticlesPolygon :: Text -> [Article]
extractArticlesPolygon html = do
  -- Parse the HTML text into an XML document.
  let doc = case Text.XML.parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  -- Create a cursor from the XML document.
  let cursor = fromDocument doc
  -- Find all article nodes by their class attribute.
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "c-entry-box--compact c-entry-box--compact--article"
  -- Extract articles from the nodes.
  mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      -- Find the URL node by its class attribute and get its href attribute.
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attributeIs "class" "c-entry-box--compact__image-wrapper" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes

      -- Find the title nodes and get their content.
      let titleNodes = articleNode $// Cursor.element "title" &/ Cursor.content
      let titleText = listToMaybe titleNodes

      -- Find the content nodes and combine their content.
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      -- Return an article if all the necessary information was found.
      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from Polygon's individual article page HTML text.
parsePolygonArticle :: Text -> [Tag Text] -> Maybe Article
parsePolygonArticle url' tags = do
  -- Find the title tags.
  titleTags <- viaNonEmpty head (sections (isTagOpenName "title") tags)
  -- Find the content tags.
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)

  -- Get the first title tag and the first content tag.
  titleTag <- viaNonEmpty head titleTags
  contentTag <- viaNonEmpty head contentTags

  -- Get the text from the tags.
  let titleText = innerText [titleTag]
  let contentText = innerText [contentTag]

  -- Return an article.
  return $ Article titleText url' contentText