{-# LANGUAGE OverloadedStrings #-}

module Scraper.GamesSutra (
  fetchGamasutraArticles,
  parseGamasutraArticle,
  extractGamasutraArticles,
  extractGamasutraTitle,
) where

import Common (Article (..))
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Scraper.Parsers ()
import Prelude hiding (error)
import Text.HTML.TagSoup
    ( sections, parseTags, isTagOpenName, innerText, fromAttrib, Tag )
import Text.XML.Cursor
    ( (&//),
      (&/),
      ($//),
      fromDocument,
      element,
      content,
      attributeIs,
      Cursor )
import qualified Data.Text
import qualified Text.HTML.DOM
import Network.HTTP.Client ( parseRequest, Response(responseBody) )


-- Update the fetchArticle function
-- Now it takes a Text parameter and returns IO (Either Text Article).
fetchArticle :: Text -> IO (Either Text Article)
fetchArticle url = do
  request <- parseRequest (toString url)
  response <- httpLBS request
  let cursor = fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ parseGamasutra url cursor

-- Update the fetchGamasutraArticles function
-- Now it returns IO (Either Text [Article]).
fetchGamasutraArticles :: IO (Either Text [Article])
fetchGamasutraArticles = do
  request <- parseRequest "https://www.gamasutra.com"
  response <- httpLBS request
  let body = decodeUtf8 $ getResponseBody response
  case parseGamasutraUrls body of
    Left error -> return $ Left error
    Right urls -> sequenceA <$> traverse fetchArticle urls


-- | 'extractGamasutraArticles' takes an HTML Text and extracts a list of 'Article's specific to Gamasutra's website.
parseGamasutraArticle :: Text -> [Tag Text] -> Maybe Article
parseGamasutraArticle url tags = do
  titleTags <- viaNonEmpty head (sections (isTagOpenName "h1") tags)
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)

  titleTag <- viaNonEmpty head titleTags
  contentTag <- viaNonEmpty head contentTags

  let titleText = innerText [titleTag]
      contentText = innerText [contentTag]

  return $ MkArticle titleText url contentText

-- Update the extractGamasutraArticles function
extractGamasutraArticles :: [Tag Text] -> [Article]
extractGamasutraArticles tags =
  let articleSections = sections (isTagOpenName "article") tags
      articles =
        mapMaybe
          ( \tags' -> do
              urlTags <- viaNonEmpty head (sections (isTagOpenName "a") tags')
              urlTag <- viaNonEmpty head urlTags
              let urlText = fromAttrib "href" urlTag
              parseWithUrl urlText tags'
          )
          articleSections
      parseWithUrl = parseGamasutraArticle
   in articles

-- Update the parseGamasutraUrls function
parseGamasutraUrls :: Text -> Either Text [Text]
parseGamasutraUrls html =
  let urls = parseUrls $ parseTags html
   in maybeToRight "Failed to parse Gamasutra URLs" urls
  where
    parseUrls :: [Tag Text] -> Maybe [Text]
    parseUrls tags = viaNonEmpty toList (fromAttrib "href" <$> concat (sections (isTagOpenName "section") tags))

-- Update the parseGamasutra function
parseGamasutra :: Text -> Cursor -> Either Text Article
parseGamasutra url cursor =
  let maybeTitle = extractGamasutraTitle cursor
      maybeContent = extractGamasutraContent cursor
   in case (maybeTitle, maybeContent) of
        (Just title, Just content) -> Right $ MkArticle title url content
        _ -> Left "Failed to parse Gamasutra article"

-- | 'extractContent' takes a 'Cursor' pointing to the root of an HTML document and extracts the main content.
extractGamasutraContent :: Cursor -> Maybe Text
extractGamasutraContent cursor = do
  let contentNodes = cursor $// element "div" >=> attributeIs "class" "article-content" &// element "p" &/ Text.XML.Cursor.content
  let contentText = Data.Text.intercalate "\n" contentNodes
  return contentText

extractGamasutraTitle :: Cursor -> Maybe Text
extractGamasutraTitle cursor = do
  let titleNodes = cursor $// element "h1" &/ Text.XML.Cursor.content
  let titleText = Data.Text.concat titleNodes
  return titleText