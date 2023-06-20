{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'when' from Relude" #-}

module ArticleHandler (
  handleNewArticle,
WebsiteHandler,
handleNewGameSpotArticle) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.Map qualified as Map
import Data.Text (isInfixOf)
import Database.Database (NewsSiteId, gamesIndustryId, gamesutraId, insertLinkIfNew, polygonId, rpsId, venturebeatId, pcgamerId, euroGamerId, gameSpotId)
import Scraper.GamesIndustry (fetchGamesIndustryArticleContent, parseGamesIndustryArticle)
import Scraper.GamesSutra (fetchGamasutraArticleContent)
import Scraper.Polygon (extractPolygonArticles, fetchPolygonArticleContent)
import Scraper.RPS (URL (..), fetchRPSArticleContent, parseRPSArticle)
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
import Scraper.PCGamer ( parsePCGArticle, fetchPCGArticleContent )
import Scraper.VentureBeat
    ( parseVBArticle, fetchVBArticleContent )
import Scraper.EuroGamer
    ( parseEuroGamerArticle, fetchEuroGamerArticleContent )
import Scraper.GameSpot
    ( parseGameSpotArticle, fetchGameSpotArticleContent )

tshow :: Show a => a -> Text
tshow = toText . (show :: Show a => a -> String)

type WebsiteHandler = Text -> NewsSiteId -> IO ()

websiteHandlers :: Map Text (Text -> NewsSiteId -> IO ())
websiteHandlers =
  Map.fromList
    [ ("gamesindustry", handleNewGamesIndustryArticle)
    , ("gamasutra", handleNewGamasutraArticle)
    , ("polygon", handleNewPolygonArticle)
    , ("rockpapershotgun", handleNewRPSArticle)
    , ("pcgamer", handleNewPCGArticle)
    , ("venturebeat", handleNewVBArticle)
    , ("eurogamer", handleNewEuroGamerArticle)
    , ("gamespot", handleNewGameSpotArticle) -- Add GameSpot here
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
  | "polygon" `isInfixOf` url' = Just <$> polygonId
  | "rockpapershotgun" `isInfixOf` url' = Just <$> rpsId
  | "pcgamer" `isInfixOf` url' = Just <$> pcgamerId
  | "venturebeat" `isInfixOf` url' = Just <$> venturebeatId
  | "eurogamer" `isInfixOf` url' = Just <$> euroGamerId
  | "gamespot" `isInfixOf` url' = Just <$> gameSpotId -- Add GameSpot here
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


-- This function extracts the website name from a URL.
siteNameFromUrl :: Text -> Text
siteNameFromUrl url'
  | "gamesindustry" `isInfixOf` url' = "gamesindustry"
  | "gamasutra" `isInfixOf` url' = "gamasutra"
  | "polygon" `isInfixOf` url' = "polygon"
  | "rockpapershotgun" `isInfixOf` url' = "rockpapershotgun"
  | "pcgamer" `isInfixOf` url' = "pcgamer"
  | "venturebeat" `isInfixOf` url' = "venturebeat"
  | "eurogamer" `isInfixOf` url' = "eurogamer"
  | "gamespot" `isInfixOf` url' = "gamespot" -- Add GameSpot here
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

-- This function processes a new article from RPS.
handleNewRPSArticle :: Text -> NewsSiteId -> IO ()
handleNewRPSArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    -- Create URL from Text
    let urll = URL url'
    -- Fetch the article content
    result <- fetchRPSArticleContent urll
    -- Handle the result
    case result of
      Left error' ->
        -- If there was an error, log it
        logError $ "Error fetching the article content: " <> tshow error'
      Right cursor -> do
        -- If the fetch was successful, parse the article
        case parseRPSArticle urll cursor of
          Nothing ->
            -- If the article couldn't be parsed, log an error
            logError "Error parsing the article."
          Just article -> do
            -- If the article was parsed successfully, analyze it
            analyzeSentimentAndTrends url' (unwords $ preprocess (content article))

-- This function processes a new article from PCGamer.
handleNewPCGArticle :: Text -> NewsSiteId -> IO ()
handleNewPCGArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchPCGArticleContent (URL url')
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right cursor -> do
        case parsePCGArticle (URL url') cursor of
          Nothing -> logError "Error parsing the article."
          Just article -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))

-- This function processes a new article from VentureBeat.
handleNewVBArticle :: Text -> NewsSiteId -> IO ()
handleNewVBArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchVBArticleContent (URL url')
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right cursor -> do
        case parseVBArticle (URL url') cursor of
          Nothing -> logError "Error parsing the article."
          Just article -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))

-- This function processes a new article from EuroGamer.
handleNewEuroGamerArticle :: Text -> NewsSiteId -> IO ()
handleNewEuroGamerArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchEuroGamerArticleContent (URL url')
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right cursor -> do
        case parseEuroGamerArticle (URL url') cursor of
          Nothing -> logError "Error parsing the article."
          Just article -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))


  
handleNewGameSpotArticle :: Text -> NewsSiteId -> IO ()
handleNewGameSpotArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    let urll = URL url' -- Convert Text to URL
    result <- fetchGameSpotArticleContent urll
    case result of
      Left error' -> logError $ "Error fetching the article content: " <> error'
      Right cursor -> do
        -- Assume fetchGameSpotArticleContent returns Cursor, not Text
        case parseGameSpotArticle urll cursor of -- Pass Cursor, not tags
          Nothing -> logError "Error parsing the article."
          Just article -> analyzeSentimentAndTrends url' (unwords $ preprocess (content article))