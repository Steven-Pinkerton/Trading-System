module Scraper.GamesIndustry (
  extractArticlesGamesIndustry,
  parseGamesIndustryArticle,
  fetchGamesIndustryArticleContent) where

import qualified Text.XML.Cursor as Cursor

import Text.XML ( parseLBS, def )
import Text.XML.Cursor (fromDocument, ($//), (&/))
import Common (Article(..) )
import Text.HTML.TagSoup
    ( sections, isTagOpenName, innerText, Tag )
import Network.HTTP.Client (Response (responseBody), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Text.HTML.DOM (parseLBS)
import Scraper.Parsers ( extractContent )
import Data.Text ()

{- | 'extractArticlesGamesIndustry' takes a Text containing an HTML document and extracts a list of 'Article's from it.
It uses 'extractArticleFromNode' internally to parse individual articles.
-}
extractArticlesGamesIndustry :: Text -> [Article]
extractArticlesGamesIndustry html = do
  let doc = case Text.XML.parseLBS def (encodeUtf8 html) of
        Left _ -> error "Failed to parse HTML"
        Right d -> d
  let cursor = fromDocument doc
  let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "article_body"
  mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let titleNodeMaybe = viaNonEmpty Prelude.head (articleNode $// Cursor.element "title" &/ Cursor.content)
      let urlNodeMaybe = viaNonEmpty Prelude.head (articleNode $// Cursor.element "a" >=> Cursor.attribute "href") -- adjust if necessary
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = Prelude.unwords contentNodes

      case (titleNodeMaybe, urlNodeMaybe) of
        (Just titleNode, Just urlNode) -> Just $ Article titleNode urlNode contentText
        _ -> Nothing

parseGamesIndustryArticle :: Text -> [Tag Text] -> Maybe Article
parseGamesIndustryArticle url' tags = do
  titleTags <- viaNonEmpty Prelude.head (sections (isTagOpenName "h1") tags)
  contentTags <- viaNonEmpty Prelude.head (sections (isTagOpenName "p") tags)

  titleTag <- viaNonEmpty Prelude.head titleTags
  contentTag <- viaNonEmpty Prelude.head contentTags

  let titleText = innerText [titleTag]
  let contentText = innerText [contentTag]

  return $ Article titleText url' contentText


-- | 'fetchArticleContent' takes a URL as input and fetches the HTML content of the page.
fetchGamesIndustryArticleContent :: Text -> IO (Either Text Text)
fetchGamesIndustryArticleContent url' = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ toString url' -- Convert Text to String for parseRequest
  response <- httpLbs request manager
  let cursor = fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
  return $ maybeToRight "Failed to extract content" (extractContent cursor)