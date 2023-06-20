{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Scraper.EuroGamer (
  URL (..),
  extractArticlesEuroGamer,
  parseEuroGamerArticle,
  fetchEuroGamerArticleContent,
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

-- | Extracts articles from EuroGamer's index page HTML text.
extractArticlesEuroGamer :: Text -> IO [Article]
extractArticlesEuroGamer html = do
  let docEither = XML.parseLBS def (Prelude.encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      let cursor = fromDocument doc
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "summary"

      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes
      let titleNodes = articleNode $// Cursor.element "p" >=> Cursor.attributeIs "class" "title" &// Cursor.element "a" &/ Cursor.content
      let titleText = listToMaybe titleNodes
      let contentNodes = articleNode $// Cursor.element "p" >=> Cursor.attributeIs "class" "strapline" &/ Cursor.content
      let contentText = unwords contentNodes

      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from EuroGamer's individual article page HTML text.
parseEuroGamerArticle :: URL -> Cursor -> Maybe Article
parseEuroGamerArticle (URL url') cursor = do
  let titleNodes = cursor $// element "title" &/ Cursor.content
  let titleText = listToMaybe titleNodes >>= viaNonEmpty head . splitOn " | "
  let contentNodes = cursor $// element "div" >=> Cursor.attributeIs "class" "article_body_content" &// element "p" &/ Cursor.content
  let contentText = unwords contentNodes

  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing

-- Fetch the HTML content of an article given its URL
fetchEuroGamerArticleContent :: URL -> IO (Either Text Cursor)
fetchEuroGamerArticleContent (URL url') = do
  response <- try $ simpleHttp (toString url') :: IO (Either SomeException BL.ByteString)
  return $ case response of
    Left _ -> Left "Error fetching the URL"
    Right html -> case parseLBS def html of
      Left _ -> Left "Error parsing HTML"
      Right doc -> Right $ fromDocument doc