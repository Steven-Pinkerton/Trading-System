{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use 'when' from Relude" #-}

module ArticleHandler (
  handleNewArticle,
) where

import ArticleExtraction.Preprocessing (preprocess)
import Common (Article (..))
import Data.Text (isInfixOf)
import Database.Database (insertLinkIfNew)
import Scraper.GamesIndustry (fetchGamesIndustryArticleContent, parseGamesIndustryArticle)
import Scraper.GamesSutra (fetchGamasutraArticleContent, parseGamasutraArticle)
import Text.HTML.TagSoup (parseTags)


handleNewGamesIndustryArticle :: Text -> IO ()
handleNewGamesIndustryArticle url' = do
  isNew <- insertLinkIfNew url'
  when isNew $ do
    result <- fetchGamesIndustryArticleContent url'
    case result of
      Left error' -> putStrLn $ toString $ "Error fetching the article content: " <> error'
      Right content' -> do
        let tags = parseTags content'
        case parseGamesIndustryArticle url' tags of
          Nothing -> putStrLn "Error parsing the article."
          Just article -> do
            let preprocessedContent = preprocess (content article)
            -- TODO: Pass preprocessedContent to your Python sentiment analysis function
            print preprocessedContent

handleNewGamasutraArticle :: Text -> IO ()
handleNewGamasutraArticle url' = do
  isNew <- insertLinkIfNew url'
  when isNew $ do
    htmlContent <- fetchGamasutraArticleContent url'
    case htmlContent of
      Left error' -> putStrLn $ toString $ "Error fetching the article content: " <> error'
      Right content' -> do
        let tags = parseTags content'
        case parseGamasutraArticle url' tags of
          Just article -> do
            -- Handle the parsed article
            print article -- For example, print the article
          Nothing -> putStrLn "Error: Unable to parse the article."

handleNewArticle :: Text -> IO ()
handleNewArticle url'
  | "gamesindustry" `isInfixOf` url' = handleNewGamesIndustryArticle url'
  | "gamasutra" `isInfixOf` url' = handleNewGamasutraArticle url'
  | otherwise = putStrLn $ "Unknown website: " ++ toString url'