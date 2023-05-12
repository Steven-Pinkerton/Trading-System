{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'mapMaybe' from Relude" #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  extractContent,
) where

import Data.ByteString.Lazy ()
import Data.Text (intercalate)
import Data.Text.Encoding ()
import Data.Text.Lazy.Encoding ()

import Common (Article (..))
import Text.HTML.TagSoup (
  Tag,
  fromAttrib,
  innerText,
  isTagOpenName,
  parseTags,
  sections,
 )
import Text.XML.Cursor (
  Cursor,
  attributeIs,
  content,
  element,
  ($//),
  (&/),
  (&//),
 )

-- | 'parseArticle' takes a list of HTML tags and extracts an 'Article' from it.
parseArticle :: [Tag Text] -> (Text -> Text -> Text -> Article) -> Maybe Article
parseArticle tags mkArticle = do
  titleTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "h1") tags)
  urlTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "a") tags)
  contentTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "p") tags)

  titleTag <- viaNonEmpty head titleTags
  urlTag <- viaNonEmpty head urlTags
  contentTag <- viaNonEmpty head contentTags

  let titletext = innerText [titleTag]
      urltext = fromAttrib "href" urlTag
      contenttext = innerText [contentTag]

  return $ mkArticle titletext urltext contenttext

-- | 'extractArticles' takes an HTML ByteString and extracts a list of 'Article's from it.
extractArticles :: ByteString -> [Article]
extractArticles html =
  let tags = parseTags (decodeUtf8 html)
      articleSections = sections (Text.HTML.TagSoup.isTagOpenName "article") tags
      articles = mapMaybe (`parseArticle` Article) articleSections
   in articles

-- | 'extractContent' takes a 'Cursor' pointing to the root of an HTML document and extracts the main content.
extractContent :: Cursor -> Maybe Text
extractContent cursor = do
  let contentNodes = cursor $// element "div" >=> attributeIs "class" "main-content" &// element "p" &/ Text.XML.Cursor.content
  let contentText = Data.Text.intercalate "\n" contentNodes
  return contentText