{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Scraper.PCGamer (
  URL (..),
extractArticlesPCG,
parsePCGArticle,
fetchPCGArticleContent) where

import Common (Article (..))
import Scraper.RPS (URL (URL))
import Text.XML (def, parseLBS)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor, element, fromDocument, ($//), (&/), (&//))
import Text.XML.Cursor qualified as Cursor
import Network.HTTP.Conduit ( simpleHttp )
import Control.Exception ( try )
import Data.ByteString.Lazy qualified as BL

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

-- Fetch the HTML content of an article given its URL
fetchPCGArticleContent :: URL -> IO (Either Text Cursor)
fetchPCGArticleContent (URL url') = do
  response <- try $ simpleHttp (toString url') :: IO (Either SomeException BL.ByteString)
  return $ case response of
    Left _ -> Left "Error fetching the URL"
    Right html -> case parseLBS def html of
      Left _ -> Left "Error parsing HTML"
      Right doc -> Right $ fromDocument doc