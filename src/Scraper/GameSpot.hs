{-# LANGUAGE OverloadedStrings #-}

module Scraper.GameSpot (
  URL (..),
  extractArticlesGameSpot,
  parseGameSpotArticle,
  fetchGameSpotArticleContent,
) where

import Common (Article (..))
import Control.Exception (try)
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Conduit (simpleHttp)
import Text.XML (def, parseLBS)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor, element, fromDocument, ($//), (&/), (&//))
import Text.XML.Cursor qualified as Cursor
import Scraper.RPS ( URL(..) )

-- | Extracts articles from GameSpot's index page HTML text.
extractArticlesGameSpot :: Text -> IO [Article]
extractArticlesGameSpot html = do
  let docEither = XML.parseLBS def (Prelude.encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      let cursor = fromDocument doc
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "card-item base-flexbox flexbox-align-center width-100 border-bottom-grayscale--thin"

      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attributeIs "class" "card-item__link text-decoration--none" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes
      let titleNodes = articleNode $// Cursor.element "h4" &/ Cursor.content
      let titleText = listToMaybe titleNodes
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing


-- | Parses an article from GameSpot's individual article page HTML text.
parseGameSpotArticle :: URL -> Cursor -> Maybe Article
parseGameSpotArticle (URL url') cursor = do
  let titleNodes = cursor $// element "title" &/ Cursor.content
  let titleText = listToMaybe titleNodes
  let contentNodes = cursor $// element "div" >=> Cursor.attributeIs "class" "js-content-entity-body content-entity-body" &// element "p" &/ Cursor.content
  let contentText = unwords contentNodes

  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing

-- Fetch the HTML content of an article given its URL
fetchGameSpotArticleContent :: URL -> IO (Either Text Cursor)
fetchGameSpotArticleContent (URL url') = do
  response <- try $ simpleHttp (toString url') :: IO (Either SomeException BL.ByteString)
  return $ case response of
    Left _ -> Left "Error fetching the URL"
    Right html -> case parseLBS def html of
      Left _ -> Left "Error parsing HTML"
      Right doc -> Right $ fromDocument doc