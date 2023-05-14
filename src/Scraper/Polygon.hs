module Scraper.Polygon (
  extractArticlesPolygon,
  parsePolygonArticle,
fetchPolygonArticleContent) where

import Text.XML.Cursor qualified as Cursor

import Common (Article (..))
import Network.HTTP.Simple (getResponseBody, httpLbs, parseRequest)
import Text.XML (def, parseLBS)
import Text.XML.Cursor
    ( fromDocument,
      ($//),
      (&/),
      fromDocument,
      element,
      ($//),
      (&/),
      element,
      fromDocument,
      ($//),
      (&/), Cursor )
import qualified Text.HTML.DOM as HTML_DOM



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
      let titleNodes = articleNode $// Cursor.element "h2" >=> Cursor.attributeIs "class" "c-entry-box--compact__title" &/ Cursor.content
      let titleText = listToMaybe titleNodes

      -- Find the content nodes and combine their content.
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      -- Return an article if all the necessary information was found.
      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from Polygon's individual article page HTML text.
parsePolygonArticle :: Text -> Cursor -> Maybe Article
parsePolygonArticle url' cursor = do
  -- Find the title and content nodes and get their content.
  let titleNodes = cursor $// element "title" &/ Cursor.content
  let contentNodes = cursor $// element "p" &/ Cursor.content

  let titleText = listToMaybe titleNodes
  let contentText = unwords contentNodes

  -- Return an article if all the necessary information was found.
  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing


fetchPolygonArticleContent :: Text -> IO (Maybe Article)
fetchPolygonArticleContent url' = do
  request <- parseRequest (toString url')
  response <- httpLbs request
  let cursor = fromDocument $ HTML_DOM.parseLBS (getResponseBody response)
  return $ parsePolygonArticle url' cursor