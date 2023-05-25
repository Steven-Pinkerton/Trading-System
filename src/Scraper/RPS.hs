{-# LANGUAGE OverloadedStrings #-}

module Scraper.RPS (
  extractArticlesRPS,
  parseRPSArticle,
  fetchRPSArticleContent,
) where

import Common (Article (..))
import Control.Exception (try)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLbs, parseRequest)
import Text.HTML.DOM qualified as HTML_DOM
import Text.XML (def, parseLBS)
import Text.XML.Cursor (
  Cursor,
  element,
  fromDocument,
  ($//),
  (&/),
  (&//),
 )
import Text.XML.Cursor qualified as Cursor

newtype URL = URL Text

-- | Extracts articles from Rock Paper Shotgun's index page HTML text.
extractArticlesRPS :: Text -> IO [Article]
extractArticlesRPS html = do
  -- Parse the HTML text into an XML document.
  let docEither = Text.XML.parseLBS def (encodeUtf8 html)
  case docEither of
    Left _ -> return []
    Right doc -> do
      -- Create a cursor from the XML document.
      let cursor = fromDocument doc

      -- Find all article nodes by their class attribute.
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "content-block content-block--article"

      -- Extract articles from the nodes.
      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      -- Find the URL node by its class attribute and get its href attribute.
      let urlNodes = articleNode $// Cursor.element "a" >=> Cursor.attributeIs "class" "card card--article" >=> Cursor.attribute "href"
      let urlText = listToMaybe urlNodes

      -- Find the title nodes and get their content.
      let titleNodes = articleNode $// Cursor.element "h2" >=> Cursor.attributeIs "class" "card__title" &/ Cursor.content
      let titleText = listToMaybe titleNodes

      -- Find the content nodes and combine their content.
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      -- Return an article if all the necessary information was found.
      case (urlText, titleText) of
        (Just url', Just title') -> Just $ Article title' url' contentText
        _ -> Nothing

-- | Parses an article from Rock Paper Shotgun's individual article page HTML text.
parseRPSArticle :: URL -> Cursor -> Maybe Article
parseRPSArticle (URL url') cursor = do
  -- Find the title node in the HTML document and get its content.
  let titleNodes = cursor $// element "title" &/ Cursor.content

  -- Find the div node with class 'article_body_content' in the HTML document,
  -- then find all nested p elements within this div and get their content.
  let contentNodes =
        cursor $// element "div" >=> Cursor.attributeIs "class" "article_body_content"
          &// element "p" &/ Cursor.content

  -- Extract the text from the first title node (if it exists).
  let titleText = listToMaybe titleNodes

  -- Concatenate all the content node texts into one text.
  let contentText = unwords contentNodes

  -- If a title was found, return an article with the found title, the given URL, and the found content.
  -- If no title was found, return Nothing.
  case titleText of
    Just title' -> Just $ Article title' url' contentText
    _ -> Nothing

-- | Fetches the content of an individual article page from Rock Paper Shotgun and
-- parses it into an 'Article' object.
fetchRPSArticleContent :: URL -> IO (Maybe Article)
fetchRPSArticleContent (URL url') = do
  -- Parse the URL into a request.
  req <- parseRequest (toString url')

  -- Send the HTTP request and get the response, handling potential HTTP exceptions.
  resEither <- (try :: IO a -> IO (Either HttpException a)) $ httpLbs req

  -- Depending on whether the HTTP request was successful or not, parse the response body into an XML document.
  return $ either (const Nothing) parseResponseBody resEither
  where
    parseResponseBody response =
      let cursor = fromDocument $ HTML_DOM.parseLBS (getResponseBody response)
      in parseRPSArticle (URL url') cursor