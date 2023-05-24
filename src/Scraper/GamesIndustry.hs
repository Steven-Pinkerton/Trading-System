module Scraper.GamesIndustry (
  extractArticlesGamesIndustry,
  parseGamesIndustryArticle,
  fetchGamesIndustryArticleContent,
) where

import Common (Article (..))
import Network.HTTP.Client (
  Response (responseBody, responseStatus),
  defaultManagerSettings,
  httpLbs,
  newManager,
  parseRequest,
 )
import Network.HTTP.Types.Status (statusCode)
import Scraper.Parsers (extractContent)
import Text.HTML.DOM (parseLBS)
import Text.HTML.TagSoup (Tag, innerText, isTagOpenName, sections)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (fromDocument, ($//), (&/))
import Text.XML.Cursor qualified as Cursor

{- | 'extractArticlesGamesIndustry' takes a Text containing an HTML document and extracts a list of 'Article's from it.
It uses 'extractArticleFromNode' internally to parse individual articles.
-}
extractArticlesGamesIndustry :: Text -> Either Text [Article]
extractArticlesGamesIndustry html = do
  let docOrError = Text.XML.parseLBS def (encodeUtf8 html)
  case docOrError of
    Left _ -> Left "Failed to parse HTML"
    Right doc -> do
      let cursor = fromDocument doc
      let articleNodes = cursor $// Cursor.element "div" >=> Cursor.attributeIs "class" "article_body"
      return $ mapMaybe extractArticleFromNode articleNodes
  where
    extractArticleFromNode articleNode = do
      let titleNodeMaybe = viaNonEmpty Prelude.head (articleNode $// Cursor.element "title" &/ Cursor.content)
      let urlNodeMaybe = viaNonEmpty Prelude.head (articleNode $// Cursor.element "a" >=> Cursor.attribute "href")
      let contentNodes = articleNode $// Cursor.element "p" &/ Cursor.content
      let contentText = Prelude.unwords contentNodes
      case (titleNodeMaybe, urlNodeMaybe) of
        (Just titleNode, Just urlNode) -> Just $ Article titleNode urlNode contentText
        _ -> Nothing

-- | 'parseGamesIndustryArticle' takes a URL and a list of HTML tags and tries to parse out an 'Article' from it.
parseGamesIndustryArticle :: Text -> [Tag Text] -> Maybe Article
parseGamesIndustryArticle url' tags = do
  titleTags <- viaNonEmpty Prelude.head (sections (isTagOpenName "h1") tags)
  contentTags <- viaNonEmpty Prelude.head (sections (isTagOpenName "p") tags)

  titleTag <- viaNonEmpty Prelude.head titleTags
  contentTag <- viaNonEmpty Prelude.head contentTags

  let titleText = innerText [titleTag]
  let contentText = innerText [contentTag]

  return $ Article titleText url' contentText

-- | 'fetchGamesIndustryArticleContent' takes a URL as input and fetches the HTML content of the page.
fetchGamesIndustryArticleContent :: Text -> IO (Either Text Text)
fetchGamesIndustryArticleContent url' = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ toString url' -- Convert Text to String for parseRequest
  response <- httpLbs request manager
  if statusCode (responseStatus response) == 200
    then do
      let cursor = fromDocument $ Text.HTML.DOM.parseLBS (responseBody response)
      return $ maybeToRight "Failed to extract content" (extractContent cursor)
    else return $ Left $ "HTTP request failed with status code: " <> show (statusCode (responseStatus response))