{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Scraper.VentureBeat (
  URL (..),
) where

import Common (Article (..))
import Text.XML (def)
import Text.XML qualified as XML
import Text.XML.Cursor (fromDocument, ($//), (&/), Cursor, element, (&//))
import Text.XML.Cursor qualified as Cursor
import Scraper.RPS (URL (URL))


-- Helper function to extract URL Text
getUrl :: URL -> Text
getUrl (URL url') = url'

-- | Extracts articles from VentureBeat's index page HTML text.
extractArticlesVB :: Text -> IO [Article]
extractArticlesVB html = do
  let docEither = XML.parseLBS def (encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      let cursor = fromDocument doc
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "vbs-layout__content"

      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes
      let titleNodes = articleNode $// Cursor.element "h2" &/ Cursor.content
      let titleText = listToMaybe titleNodes
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from VentureBeat's individual article page HTML text.
parseVBArticle :: URL -> Cursor -> Maybe Article
parseVBArticle (URL url') cursor = do
  let titleNodes = cursor $// element "h1" &/ Cursor.content
  let contentNodes = cursor $// element "div" >=> Cursor.attributeIs "class" "article-content" &// element "p" &/ Cursor.content
  let titleText = listToMaybe titleNodes
  let contentText = unwords contentNodes

  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing