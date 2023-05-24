{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use alternative" #-}

-- | Module for scraping articles from the Gamasutra website
module Scraper.GamesSutra (
  -- * Exported Functions
  fetchGamasutraArticles,
  parseGamasutraArticle,
  extractGamasutraArticles,
  extractGamasutraTitle,
  fetchGamasutraArticleContent,
) where

import Common (Article (..)) -- Import the 'Article' data type from the Common module
import Control.Exception (try) -- Used to handle exceptions

-- Used to get the response body from HTTP requests
-- Libraries for making HTTP requests
-- Parser functions for scraping

-- Libraries for parsing HTML

-- Used for parsing XML data
import Data.ByteString.Lazy qualified as L -- Used for dealing with ByteStrings of arbitrary length
import Data.Text qualified as T
import Network.HTTP.Client (Response (responseBody))
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Text.HTML.DOM qualified
import Text.HTML.TagSoup (
  Tag,
  fromAttrib,
  innerText,
  isTagOpenName,
  parseTags,
  sections,
 )
import Text.XML.Cursor qualified as C

-- Fetch the content of an article from Gamasutra
fetchGamasutraArticleContent :: T.Text -> IO (Either T.Text T.Text)
fetchGamasutraArticleContent url' = do
  request <- parseRequest (toString url')
  result <- try $ httpLBS request :: IO (Either SomeException (Response L.ByteString))
  case result of
    Left ex -> return $ Left $ show ex -- If there's an exception, return it
    Right response -> do
      let cursor = C.fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
      return $ case parseGamasutra url' cursor of
        Right article -> Right $ content article -- If the article is parsed correctly, return its content
        Left error' -> Left error' -- If there's an error, return it

-- Fetch an article from Gamasutra
fetchArticle :: T.Text -> IO (Either T.Text Article)
fetchArticle url' = do
  request <- parseRequest (toString url')
  response <- httpLBS request
  let cursor = C.fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ parseGamasutra url' cursor -- Return the parsed article

-- Fetch all articles from Gamasutra
fetchGamasutraArticles :: IO (Either T.Text [Article])
fetchGamasutraArticles = do
  request <- parseRequest "https://www.gamasutra.com"
  response <- httpLBS request
  let body = Prelude.decodeUtf8 $ getResponseBody response
  case parseGamasutraUrls body of
    Left error' -> return $ Left error' -- If there's an error parsing the URLs, return it
    Right urls -> sequenceA <$> traverse fetchArticle urls -- Fetch each article from the parsed URLs

-- Parse a Gamasutra article
parseGamasutraArticle :: Text -> [Tag Text] -> Maybe Article
parseGamasutraArticle url' tags = do
  titleTags <- viaNonEmpty head (sections (isTagOpenName "h1") tags)
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)
  titleTag <- viaNonEmpty head titleTags
  contentTag <- viaNonEmpty head contentTags
  let titleText = innerText [titleTag]
      contentText = innerText [contentTag]
  return $ Article titleText url' contentText -- Return the parsed article

-- Extract articles from Gamasutra
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

-- Parse Gamasutra URLs
parseGamasutraUrls :: T.Text -> Either T.Text [T.Text]
parseGamasutraUrls html =
  let urls = parseUrls $ parseTags html
   in if null urls then Right [] else maybeToRight "Failed to parse Gamasutra URLs" (viaNonEmpty toList urls)
  where
    parseUrls :: [Tag T.Text] -> [T.Text]
    parseUrls tags =
      fromAttrib "href" <$> concat (sections (isTagOpenName "section") tags) -- Extract the href attribute from each section tag

-- Parse Gamasutra
parseGamasutra :: Text -> C.Cursor -> Either Text Article
parseGamasutra url' cursor =
  let maybeTitle = extractGamasutraTitle cursor
      maybeContent = extractGamasutraContent cursor
   in case (maybeTitle, maybeContent) of
        (Just title', Just content') -> Right $ Article title' url' content' -- If the title and content are parsed correctly, return the article
        (Nothing, Nothing) -> Left "Failed to parse both title and content from Gamasutra article"
        (Nothing, _) -> Left "Failed to parse title from Gamasutra article"
        (_, Nothing) -> Left "Failed to parse content from Gamasutra article"

-- Extract the content of an article from Gamasutra
extractGamasutraContent :: C.Cursor -> Maybe T.Text
extractGamasutraContent cursor = do
  let contentNodes = cursor C.$// C.element "div" C.&| C.attributeIs "class" "article-content" C.&// C.element "p" C.&/ C.content
  let contentText = T.intercalate "\n" $ map T.concat contentNodes -- Concatenate the content nodes
  return contentText

-- Extract the title of an article from Gamasutra
extractGamasutraTitle :: C.Cursor -> Maybe T.Text
extractGamasutraTitle cursor = do
  let titleNodes = cursor C.$// C.element "h1" C.&/ C.content
  let titleText = T.concat titleNodes -- Concatenate the title nodes
  return titleText