{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'mapMaybe' from Relude" #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  extractArticlesGamesIndustry,
  fetchArticleContent,
  extractContent,
) where

import Data.ByteString.Lazy ()
import Data.Text (intercalate)
import Data.Text.Encoding ()
import Data.Text.Lazy.Encoding ()
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, Response (responseBody))
import Text.HTML.DOM (parseLBS)
import ArticleExtraction.Article ( Article(Article) )
import qualified Text.XML
import qualified Text.XML.Cursor as Cursor
import Text.XML.Cursor
    ( attributeIs,
      content,
      element,
      fromDocument,
      ($//),
      (&/),
      (&//),
      Cursor )
import Text.XML (def)
import Text.HTML.TagSoup
    ( sections, parseTags, innerText, fromAttrib, Tag, isTagOpenName )


{- | 'parseArticle' takes a list of HTML tags and extracts an 'Article' from it.
 You may need to modify this function to suit the structure of your target websites.
-}
parseArticle :: [Tag Text] -> Maybe Article
parseArticle tags = do
  titleTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "h1") tags)
  urlTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "a") tags)
  contentTags <- viaNonEmpty head (sections (Text.HTML.TagSoup.isTagOpenName "p") tags)

  titleTag <- viaNonEmpty head titleTags
  urlTag <- viaNonEmpty head urlTags
  contentTag <- viaNonEmpty head contentTags

  let titletext = innerText [titleTag]
      urltext = fromAttrib "href" urlTag
      contenttext = innerText [contentTag]

  return $ Article titletext urltext contenttext

{- | 'extractArticles' takes an HTML ByteString and extracts a list of 'Article's from it.
 It uses 'parseArticle' internally to parse individual articles.
-}
extractArticles :: ByteString -> [Article]
extractArticles html =
  let tags = parseTags (decodeUtf8 html)
      articleSections = sections (Text.HTML.TagSoup.isTagOpenName "article") tags
      articles = mapMaybe parseArticle articleSections
   in articles


{- | 'extractArticlesGamesIndustry' takes a Text containing an HTML document and extracts a list of 'Article's from it.
It uses 'extractArticleFromNode' internally to parse individual articles.
-}
extractArticlesGamesIndustry :: Text -> [Article]
extractArticlesGamesIndustry html = do
  let doc = case Text.XML.parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  let cursor = fromDocument doc
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "summary"
  mapMaybe extractArticleFromNode articleNodes
  where
    {- | 'extractArticleFromNode' is a local helper function for 'extractArticlesGamesIndustry'.
     It takes an 'articleNode' as input, which is a cursor pointing to an individual article
     in the HTML structure. The function extracts the title, URL, and content from the node
     and returns a 'Maybe Article'. If the title and URL can be extracted successfully, it
     returns 'Just Article'; otherwise, it returns 'Nothing'.
    -}
    extractArticleFromNode articleNode = do
      let titleNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "title")
      let urlNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "href")
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (titleNodeMaybe, urlNodeMaybe) of
        (Just titleNode, Just urlNode) -> Just $ Article titleNode urlNode contentText
        _ -> Nothing

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