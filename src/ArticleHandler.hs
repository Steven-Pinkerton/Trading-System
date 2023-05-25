{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'when' from Relude" #-}

module ArticleHandler (
  handleNewArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.Map qualified as Map
import Data.Text (isInfixOf)
import Database.Database (NewsSiteId, gamesIndustryId, gamesutraId, insertLinkIfNew, polygonId, RspId)
import Scraper.GamesIndustry (fetchGamesIndustryArticleContent, parseGamesIndustryArticle)
import Scraper.GamesSutra (fetchGamasutraArticleContent)
import Scraper.Polygon (extractPolygonArticles, fetchPolygonArticleContent)
import SentimentAnalysis.PythonScript (
  callPythonScript,
  parseSentimentOutput,
 )
import Text.HTML.TagSoup (parseTags)
import TrendAnalysis.PythonScript (
  callPythonTrendScript,
  parseTrendingOutput,
 )
import SentimentAnalysis.Sentiment ( sentimentToText )

type WebsiteHandler = Text -> NewsSiteId -> IO ()

websiteHandlers :: Map.Map Text WebsiteHandler
websiteHandlers =
  Map.fromList
    [ ("gamesindustry", handleNewGamesIndustryArticle)
    , ("gamasutra", handleNewGamasutraArticle)
    , ("polygon", handleNewPolygonArticle)
    ]

-- This function processes a new article from GamesIndustry.
handleNewGamesIndustryArticle :: Text -> NewsSiteId -> IO ()
handleNewGamesIndustryArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchGamesIndustryArticleContent url'
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right content' -> do
        let tags = parseTags content'
        case parseGamesIndustryArticle url' tags of
          Nothing -> logError "Error parsing the article."
          Just article -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))

-- This function processes a new article from Gamasutra.
handleNewGamasutraArticle :: Text -> NewsSiteId -> IO ()
handleNewGamasutraArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    htmlContent <- fetchGamasutraArticleContent url'
    case htmlContent of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right content' -> analyzeSentimentAndTrends url' (unwords $ preprocess content')

-- This function selects the right handler function for a new article based on its URL.
handleNewArticle :: Text -> IO ()
handleNewArticle url' = do
  maybeSiteId <- newsSiteIdFromUrl url'
  case maybeSiteId of
    Nothing -> logError $ "Unknown website: " <> url'
    Just siteId ->
      let handler = Map.lookup (siteNameFromUrl url') websiteHandlers
       in maybe
            (logError $ "No handler for website: " <> url')
            (\h -> h url' siteId)
            handler


-- This function decides the right NewsSiteId for a URL.
newsSiteIdFromUrl :: Text -> IO (Maybe NewsSiteId)
newsSiteIdFromUrl url'
  | "gamesindustry" `isInfixOf` url' = Just <$> gamesIndustryId
  | "gamasutra" `isInfixOf` url' = Just <$> gamesutraId
  | "polygon" `isInfixOf` url' = Just <$> polygonId -- you will need to define polygonId
  | otherwise = return Nothing

-- This function logs an error message.
logError :: Text -> IO ()
logError = putStrLn . toString -- Assuming unpack is imported from Data.Text

-- This function runs sentiment analysis and trend analysis on the preprocessed content of an article.
analyzeSentimentAndTrends :: Text -> Text -> IO ()
analyzeSentimentAndTrends url' preprocessedContent = do
  rawSentimentOutput <- callPythonScript preprocessedContent
  let sentiment = parseSentimentOutput rawSentimentOutput
  print sentiment
  when (sentimentToText sentiment == "negative") $ do
    rawTrendingOutput <- callPythonTrendScript url' preprocessedContent (show sentiment) -- Using show to convert Sentiment to String
    let trendingTopics = parseTrendingOutput rawTrendingOutput
    print trendingTopics


-- Note: This function should be implemented to extract the website name from a URL.
siteNameFromUrl :: Text -> Text
siteNameFromUrl url'
  | "gamesindustry" `isInfixOf` url' = "gamesindustry"
  | "gamasutra" `isInfixOf` url' = "gamasutra"
  | "polygon" `isInfixOf` url' = "polygon"
  | otherwise = "unknown"


-- This function processes a new article from Polygon.
handleNewPolygonArticle :: Text -> NewsSiteId -> IO ()
handleNewPolygonArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchPolygonArticleContent url'
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right content' -> do
        let tags = parseTags content'
        let articles = extractPolygonArticles url' tags
        case articles of
          [] -> logError "Error parsing the article."
          (article : _) -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))
