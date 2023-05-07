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
) where


import Data.ByteString.Lazy ()
import Data.Text (intercalate)
import Data.Text.Encoding ()
import Data.Text.Lazy.Encoding ()
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, Response (responseBody))
import Text.HTML.DOM (parseLBS)

import Text.XML.Cursor
    ( attributeIs,
      content,
      element,
      fromDocument,
      ($//),
      (&/),
      (&//),
      Cursor )
import Text.HTML.TagSoup
    ( sections, parseTags, innerText, fromAttrib, Tag, isTagOpenName)
import Common (Article (MkArticle))

{- | 'parseArticle' takes a list of HTML tags and extracts an 'Article' from it.
 You may need to modify this function to suit the structure of your target websites.
-}
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

{- | 'extractArticles' takes an HTML ByteString and extracts a list of 'Article's from it.
 It uses 'parseArticle' internally to parse individual articles.
-}
extractArticles :: ByteString -> [Article]
extractArticles html =
  let tags = parseTags (decodeUtf8 html)
      articleSections = sections (Text.HTML.TagSoup.isTagOpenName "article") tags
      articles = mapMaybe (`parseArticle` MkArticle) articleSections
   in articles

{- | 'extractArticlesGamesIndustry' takes a Text containing an HTML document and extracts a list of 'Article's from it.
It uses 'extractArticleFromNode' internally to parse individual articles.
-}

{- | 'fetchArticleContent' takes a URL as input and fetches the HTML content of the page.
 It then uses 'extractContent' to extract the main content of the page, returning a 'Maybe Text'.
-}
fetchArticleContent :: String -> IO (Maybe Text)
fetchArticleContent url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  let cursor = fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ extractContent cursor


{- | 'extractContent' takes a 'Cursor' pointing to the root of an HTML document and extracts the main content.
 It looks for a 'div' element with a "class" attribute of "main-content", then collects all 'p' elements
 within that 'div'. The content is concatenated with newline characters and returned as a 'Maybe Text'.
-}
extractContent :: Cursor -> Maybe Text
extractContent cursor = do
  let contentNodes = cursor $// element "div" >=> attributeIs "class" "main-content" &// element "p" &/ content
  let contentText = Data.Text.intercalate "\n" contentNodes
  return contentText


{- | 'extractGamasutraArticles' takes an HTML Text and extracts a list of 'Article's specific to Gamasutra's website.
 It uses 'parseGamasutraArticle' internally to parse individual articles.
-}
extractGamasutraArticles :: [Tag Text] -> [Article]
extractGamasutraArticles tags =
  let articleSections = sections (isTagOpenName "article") tags
      articles = mapMaybe parseGamasutraArticle articleSections
   in articles

{- | 'parseGamasutraArticle' takes a list of HTML tags specific to Gamasutra's website and extracts an 'Article' from it.
 You may need to modify this function to suit the structure of the Gamasutra website.
-}
parseGamasutraArticle :: [Tag Text] -> Maybe Article
parseGamasutraArticle tags = do
  titleTags <- viaNonEmpty head (sections (isTagOpenName "h1") tags)
  urlTags <- viaNonEmpty head (sections (isTagOpenName "a") tags)
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)

  titleTag <- viaNonEmpty head titleTags
  urlTag <- viaNonEmpty head urlTags
  contentTag <- viaNonEmpty head contentTags

  let titleText = innerText [titleTag]
      urlText = fromAttrib "href" urlTag
      contentText = innerText [contentTag]

  return $ MkArticle titleText urlText contentText