{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Scraper.PCGamer (
  URL (..),
) where

import Common (Article (..))
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text.Encoding (encodeUtf8)
import Scraper.RPS (URL (URL))
import Text.XML (def)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor, element, fromDocument, ($//), (&/), (&//))
import Text.XML.Cursor qualified as Cursor

-- Helper function to extract URL Text
getUrl :: URL -> Text
getUrl (URL url') = url'

-- | Extracts articles from PCGamer's index page HTML text.
extractArticlesPCG :: Text -> IO [Article]
extractArticlesPCG html = do
  let docEither = XML.parseLBS def (Prelude.encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      let cursor = fromDocument doc
      -- Replace with appropriate class names and elements once known.
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "vbs-layout__content"

      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      -- Replace with appropriate class names and elements once known.
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes
      let titleNodes = articleNode $// Cursor.element "h2" &/ Cursor.content
      let titleText = listToMaybe titleNodes
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from PCGamer's individual article page HTML text.
parsePCGArticle :: URL -> Cursor -> Maybe Article
parsePCGArticle (URL url') cursor = do
  -- Replace with appropriate class names and elements once known.
  let titleNodes = cursor $// element "h1" &/ Cursor.content
  let contentNodes = cursor $// element "div" >=> Cursor.attributeIs "class" "article-content" &// element "p" &/ Cursor.content
  let titleText = listToMaybe titleNodes
  let contentText = unwords contentNodes

  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing