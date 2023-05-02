{-# LANGUAGE OverloadedStrings #-}

module Scraper.Parsers (
  parseArticle,
  extractArticles,
  extractArticlesGamesIndustry,
) where

import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, sections)
import Common (Article (..))
import Control.Lens (view, (&), (.~), (^.), (^..))
import Text.XML (parseLBS_)
import Text.XML.Lens (attribute, contents, eachInClass, element)

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
  let doc = parseLBS_ html
  let articleNodes = doc ^.. eachInClass "summary"
  map extractArticleFromNode articleNodes
  where
    extractArticleFromNode node = do
      let titleNode = node ^. element "a" . attribute "title"
      let urlNode = node ^. element "a" . attribute "href"
      let contentNodes = node ^.. eachInClass "strapline" . element "p" . contents
      let contentText = concat contentNodes
      Article titleNode urlNode contentText