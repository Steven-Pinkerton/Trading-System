{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'when' from Relude" #-}

module ArticleHandler (
  handleNewArticle,
) where

import Control.Monad (when)
import Database.Database (insertLinkIfNew)
import Scraper.GamesIndustry (fetchGamesIndustyArticleContent, parseGamesIndustryArticle)
import Scraper.GamesSutra (fetchGamasutraArticleContent, parseGamasutraArticle)
import Text.HTML.TagSoup (parseTags)

handleNewGamesIndustryArticle :: Text -> IO ()
handleNewGamesIndustryArticle url = do
  isNew <- insertLinkIfNew url
  when isNew $ do
    result <- fetchGamesIndustryArticleContent url
    case result of
      Left error -> putStrLn $ "Error fetching the article content: " <> error
      Right content -> do
        let tags = parseTags content
        case parseGamesIndustryArticle url tags of
          Nothing -> putStrLn "Error parsing the article."
          Just article -> do
            let preprocessedContent = preprocess (articleContent article)
            -- TODO: Pass preprocessedContent to your Python sentiment analysis function
            print preprocessedContent


handleNewGamasutraArticle :: Text -> IO ()
handleNewGamasutraArticle url = do
  isNew <- insertLinkIfNew url
  when isNew $ do
    htmlContent <- fetchGamasutraArticleContent url
    case htmlContent of
      Just content -> do
        let tags = parseTags content
        case parseGamasutraArticle url tags of
          Just article -> do
            -- Handle the parsed article
            print article -- For example, print the article
          Nothing -> putStrLn "Error: Unable to parse the article."
      Nothing -> putStrLn "Error: Unable to fetch the article content."

handleNewArticle :: Text -> IO ()
handleNewArticle url
  | "gamesindustry" `isInfixOf` url = handleNewGamesIndustryArticle url
  | "gamasutra" `isInfixOf` url = handleNewGamasutraArticle url
  | otherwise = putStrLn $ "Unknown website: " ++ toString url