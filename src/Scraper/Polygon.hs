{-# LANGUAGE OverloadedStrings #-}

module Scraper.Polygon (
  fetchPolygonArticles,
  parsePolygonArticle,
  fetchPolygonArticleContent,
extractPolygonArticles) where

import Common (Article (..))
import Control.Exception (try)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Network.HTTP.Client (Response (responseBody))
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Text.HTML.DOM qualified
import Text.XML.Cursor qualified as C
import Text.HTML.TagSoup
    ( parseTags, Tag, isTagOpenName, fromAttrib, sections, renderTags )

-- Fetch the content of an article from Polygon
fetchPolygonArticleContent :: T.Text -> IO (Either T.Text T.Text)
fetchPolygonArticleContent url' = do
  request <- parseRequest (toString url')
  result <- try $ httpLBS request :: IO (Either SomeException (Response L.ByteString))
  case result of
    Left ex -> return $ Left $ show ex
    Right response -> do
      let cursor = C.fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
      return $ case parsePolygonArticle url' cursor of
        Right article -> Right $ content article
        Left error' -> Left error'

-- Fetch an article from Polygon
fetchPolygonArticle :: T.Text -> IO (Either T.Text Article)
fetchPolygonArticle url' = do
  request <- parseRequest (toString url')
  response <- httpLBS request
  let cursor = C.fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ parsePolygonArticle url' cursor

-- Fetch all articles from Polygon
fetchPolygonArticles :: IO (Either T.Text [Article])
fetchPolygonArticles = do
  request <- parseRequest "https://www.polygon.com"
  response <- httpLBS request
  let body = Prelude.decodeUtf8 $ getResponseBody response
  case parsePolygonUrls body of
    Left error' -> return $ Left error'
    Right urls -> sequenceA <$> traverse fetchPolygonArticle urls

-- Extract articles from Polygon
-- Extract articles from Polygon
extractPolygonArticles :: Text -> [Tag Text] -> [Article]
extractPolygonArticles _ tags =
  let articleSections = sections (isTagOpenName "article") tags
      articles =
        mapMaybe
          ( \tags' -> do
              urlTags <- viaNonEmpty head (sections (isTagOpenName "a") tags')
              urlTag <- viaNonEmpty head urlTags
              let urlText = fromAttrib "href" urlTag
              let htmlText = renderTags tags' -- convert tags back to text
              let doc = Text.HTML.DOM.parseLBS $ encodeUtf8 htmlText
              let cursor = C.fromDocument doc
              case parsePolygonArticle urlText cursor of
                Right article -> Just article
                Left _ -> Nothing
          )
          articleSections
   in articles

-- Parse Polygon URLs
parsePolygonUrls :: T.Text -> Either T.Text [T.Text]
parsePolygonUrls html =
  let urls = parseUrls $ parseTags html
   in if null urls then Right [] else maybeToRight "Failed to parse Polygon URLs" (viaNonEmpty toList urls)
  where
    parseUrls :: [Tag T.Text] -> [T.Text]
    parseUrls tags =
      fromAttrib "href" <$> concat (sections (isTagOpenName "section") tags)

-- Parse a Polygon article
parsePolygonArticle :: Text -> C.Cursor -> Either Text Article
parsePolygonArticle url' cursor =
  let maybeTitle = extractPolygonTitle cursor
      maybeContent = extractPolygonContent cursor
   in case (maybeTitle, maybeContent) of
        (Just title', Just content') -> Right $ Article title' url' content'
        (Nothing, Nothing) -> Left "Failed to parse both title and content from Polygon article"
        (Nothing, _) -> Left "Failed to parse title from Polygon article"
        (_, Nothing) -> Left "Failed to parse content from Polygon article"

-- Extract the content of an article from Polygon
extractPolygonContent :: C.Cursor -> Maybe T.Text
extractPolygonContent cursor = do
  let contentNodes = cursor C.$// C.element "div" C.&| C.attributeIs "class" "c-entry-content" C.&// C.element "p" C.&/ C.content
  let contentText = T.intercalate "\n" $ map T.concat contentNodes
  return contentText

-- Extract the title of an article from Polygon
extractPolygonTitle :: C.Cursor -> Maybe T.Text
extractPolygonTitle cursor = do
  let titleNodes = cursor C.$// C.element "h1" C.&/ C.content
  let titleText = T.concat titleNodes
  return titleText
