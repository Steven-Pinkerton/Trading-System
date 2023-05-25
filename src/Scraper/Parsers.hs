{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'mapMaybe' from Relude" #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  extractContent,
) where

import Common (Article (..))
import Data.Text (intercalate)
import Text.HTML.TagSoup (
  Tag,
  fromAttrib,
  innerText,
  isTagOpenName,
  parseTags,
  sections,
 )
import Text.XML.Cursor (
  attributeIs,
  ($//),
  (&/),
  (&//),
 )
import Text.XML.Cursor qualified as C



data RawArticle = RawArticle {rawTitle :: Text, rawUrl :: Text, rawContent :: Text}

-- Helper function to factor out common pattern
extractTags :: String -> [Tag Text] -> Maybe [Tag Text]
extractTags tagName tags =
  viaNonEmpty head $ sections (isTagOpenName $ toText tagName) tags

-- | 'parseArticle' takes a list of HTML tags and extracts a 'RawArticle' from it.
parseArticle :: [Tag Text] -> Maybe RawArticle
parseArticle tags = do
  -- Extract title, url, and content tags from HTML tags
  titleTags <- extractTags "h1" tags
  urlTags <- extractTags "a" tags
  contentTags <- extractTags "p" tags

  -- Extract single tag from each list of tags
  titleTag <- viaNonEmpty head titleTags
  urlTag <- viaNonEmpty head urlTags
  contentTag <- viaNonEmpty head contentTags

  -- Extract inner text and href attribute
  let titletext = innerText [titleTag]
      urltext = fromAttrib "href" urlTag
      contenttext = innerText [contentTag]

  -- Construct raw article
  return $ RawArticle titletext urltext contenttext

-- | 'extractArticles' takes an HTML ByteString and extracts a list of 'Article's from it.
extractArticles :: ByteString -> [Article]
extractArticles html =
  let tags = parseTags (decodeUtf8 html)
      articleSections = sections (isTagOpenName "article") tags
      rawArticles = mapMaybe parseArticle articleSections
   in map (\rawArticle -> Article (rawTitle rawArticle) (rawUrl rawArticle) (rawContent rawArticle)) rawArticles

-- | 'extractContent' takes a 'Cursor' pointing to the root of an HTML document and extracts the main content.
extractContent :: C.Cursor -> Maybe Text
extractContent cursor = do
  -- Extract content nodes from cursor
  let contentNodes = cursor $// C.element "div" >=> attributeIs "class" "main-content" &// C.element "p" &/ C.content
  -- Intercalate content nodes with newline characters
  let contentText = Data.Text.intercalate "\n" contentNodes
  return contentText