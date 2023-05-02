{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'mapMaybe' from Relude" #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  extractArticlesGamesIndustry,
) where

import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, sections)
import Common (Article (..))
import Text.XML (parseLBS, def)
import Text.XML.Cursor qualified as Cursor
import Text.XML.Cursor ( fromDocument, ($//), (&/) )

{- | 'parseArticle' takes a list of HTML tags and extracts an 'Article' from it.
 You may need to modify this function to suit the structure of your target websites.
-}
parseArticle :: [Tag Text] -> Maybe Article
parseArticle tags = do
  titleTags <- viaNonEmpty head (sections (isTagOpenName "h1") tags)
  urlTags <- viaNonEmpty head (sections (isTagOpenName "a") tags)
  contentTags <- viaNonEmpty head (sections (isTagOpenName "p") tags)

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
      articleSections = sections (isTagOpenName "article") tags
      articles = mapMaybe parseArticle articleSections
   in articles

isTagOpenName :: Text -> Tag Text -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

extractArticlesGamesIndustry :: Text -> [Article]
extractArticlesGamesIndustry html = do
  let doc = case parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  let cursor = fromDocument doc
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "summary"
  mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let titleNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "title")
      let urlNodeMaybe = viaNonEmpty head (articleNode $// Cursor.element "a" >=> Cursor.attribute "href")
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = unwords contentNodes

      case (titleNodeMaybe, urlNodeMaybe) of
        (Just titleNode, Just urlNode) -> Just $ Article titleNode urlNode contentText
        _ -> Nothing