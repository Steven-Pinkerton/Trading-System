{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'mapMaybe' from Relude" #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  fetchArticleContent,
  extractContent,
  parseGamasutraArticle,
  extractGamasutraArticles,
  parseGamasutraUrls,
  parseGamasutra,
) where

import Data.ByteString.Lazy ()
import Data.Text (intercalate)
import Data.Text.Encoding ()
import Data.Text.Lazy.Encoding ()
import Network.HTTP.Client (Response (responseBody), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Text.HTML.DOM (parseLBS)

import Common (Article (MkArticle))
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
  fromDocument,
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
      articles = mapMaybe (`parseArticle` MkArticle) articleSections
   in articles

-- | 'fetchArticleContent' takes a URL as input and fetches the HTML content of the page.
fetchArticleContent :: String -> IO (Maybe Text)
fetchArticleContent url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  let cursor = fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ extractContent cursor

-- | 'extractContent' takes a 'Cursor' pointing to the root of an HTML document and extracts the main content.
extractContent :: Cursor -> Maybe Text
extractContent cursor = do
  let contentNodes = cursor $// element "div" >=> attributeIs "class" "main-content" &// element "p" &/ content
  let contentText = Data.Text.intercalate "\n" contentNodes
  return contentText

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
      parseWithUrl url = parseGamasutraArticle url
      articles =
        mapMaybe
          ( \tags' -> do
              urlTags <- viaNonEmpty head (sections (isTagOpenName "a") tags')
              urlTag <- viaNonEmpty head urlTags
              let urlText = fromAttrib "href" urlTag
              parseWithUrl urlText tags'
          )
          articleSections
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
parseGamasutra :: Text -> Text -> Either Text Article
parseGamasutra url html =
  let article = parseGamasutraArticle url $ parseTags html
   in maybeToRight "Failed to parse Gamasutra article" article