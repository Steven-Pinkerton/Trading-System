{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Scraper.Kotaku (
  URL (..),
  extractArticlesKotaku,
  parseKotakuArticle,
  fetchKotakuArticleContent,
) where

import Common (Article (..))
import Control.Exception (try)
import Data.ByteString.Lazy qualified as BL
import Data.Text (splitOn)
import Network.HTTP.Conduit (simpleHttp)
import Scraper.RPS (URL (URL))
import Text.XML (def, parseLBS)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor, element, fromDocument, ($//), (&/), (&//))
import Text.XML.Cursor qualified as Cursor

-- | Extracts articles from Kotaku's index page HTML text.
extractArticlesKotaku :: Text -> IO [Article]
extractArticlesKotaku html = do
  let docEither = XML.parseLBS def (Prelude.encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      let cursor = fromDocument doc
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "sc-1op1dd6-0 bRYFFT js_sticky-top-ad"

      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes
      let titleNodes = articleNode $// Cursor.element "h1" &/ Cursor.content
      let titleText = listToMaybe titleNodes
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from Kotaku's individual article page HTML text.
parseKotakuArticle :: URL -> Cursor -> Maybe Article
parseKotakuArticle (URL url') cursor = do
  let titleNodes = cursor $// element "h1" &/ Cursor.content
  let titleText = listToMaybe titleNodes >>= viaNonEmpty head . splitOn " | "
  let contentNodes = cursor $// element "p" &/ Cursor.content
  let contentText = unwords contentNodes

  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing

-- Fetch the HTML content of an article given its URL
fetchKotakuArticleContent :: URL -> IO (Either Text Cursor)
fetchKotakuArticleContent (URL url') = do
  response <- try $ simpleHttp (toString url') :: IO (Either SomeException BL.ByteString)
  return $ case response of
    Left _ -> Left "Error fetching the URL"
    Right html -> case parseLBS def html of
      Left _ -> Left "Error parsing HTML"
      Right doc -> Right $ fromDocument doc