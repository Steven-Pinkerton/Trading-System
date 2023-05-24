{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'when' from Relude" #-}

module ArticleHandler (
  handleNewArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.Text (isInfixOf)
import Database.Database (NewsSiteId, gamesIndustryId, gamesutraId, insertLinkIfNew)
import Scraper.GamesIndustry (fetchGamesIndustryArticleContent, parseGamesIndustryArticle)
import Scraper.GamesSutra (fetchGamasutraArticleContent)
import SentimentAnalysis.PythonScript (
  callPythonScript,
  parseSentimentOutput,
 )
import Text.HTML.TagSoup (parseTags)
import TrendAnalysis.PythonScript (
  callPythonTrendScript, -- To be implemented
  parseTrendingOutput,
 )

handleNewGamesIndustryArticle :: Text -> NewsSiteId -> IO ()
handleNewGamesIndustryArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    result <- fetchGamesIndustryArticleContent url'
    case result of
      Left error' -> putStrLn $ toString $ "Error fetching the article content: " <> error'
      Right content' -> do
        let tags = parseTags content'
        case parseGamesIndustryArticle url' tags of
          Nothing -> putStrLn "Error parsing the article."
          Just article -> do
            let preprocessedContent = unwords $ preprocess (content article)
            rawSentimentOutput <- callPythonScript preprocessedContent
            let sentiment = parseSentimentOutput rawSentimentOutput
            print sentiment
            when (sentiment == "negative") $ do
              rawTrendingOutput <- callPythonTrendScript url' preprocessedContent sentiment -- pass sentiment here
              let trendingTopics = parseTrendingOutput rawTrendingOutput
              print trendingTopics


handleNewGamasutraArticle :: Text -> NewsSiteId -> IO ()
handleNewGamasutraArticle url' siteId = do
  isNew <- insertLinkIfNew url' siteId
  when isNew $ do
    htmlContent <- fetchGamasutraArticleContent url'
    case htmlContent of
      Left error' -> putStrLn $ toString $ "Error fetching the article content: " <> error'
      Right content' -> do
        let preprocessedContent = preprocess content' -- assuming preprocess function exists for this content type
        rawSentimentOutput <- callPythonScript (unwords preprocessedContent)
        let sentiment = parseSentimentOutput rawSentimentOutput
        print sentiment

handleNewArticle :: Text -> IO ()
handleNewArticle url' = do
  maybeSiteId <- newsSiteIdFromUrl url'
  case maybeSiteId of
    Nothing -> putStrLn $ "Unknown website: " ++ toString url'
    Just siteId ->
      if "gamesindustry" `isInfixOf` url'
        then handleNewGamesIndustryArticle url' siteId
        else
          if "gamasutra" `isInfixOf` url'
            then handleNewGamasutraArticle url' siteId
            else putStrLn $ "Unknown website: " ++ toString url'

        
newsSiteIdFromUrl :: Text -> IO (Maybe NewsSiteId)
newsSiteIdFromUrl url'
  | "gamesindustry" `isInfixOf` url' = Just <$> gamesIndustryId
  | "gamasutra" `isInfixOf` url' = Just <$> gamesutraId
  | otherwise = return Nothing

