module Polling (startPolling) where

import Relude ( ConvertUtf8(decodeUtf8) )
import ArticleHandler (handleNewArticle)
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad ()
import Database.Database (getAllLinks, getNewsSiteId, insertLinkIfNew)
import Network.HTTP.Simple ( getResponseBody, httpBS )
import Text.HTML.TagSoup ( parseTags, fromAttrib, isTagOpenName )

startPolling :: [String] -> [String] -> IO ()
startPolling sites urls = forM_ urls $ \url -> forkIO $
  runInfinitely $ do
    response <- httpBS (fromString url)
    let tags = parseTags (getResponseBody response)
        links = [Relude.decodeUtf8 (fromAttrib "href" tag) |
           tag <- tags, isTagOpenName "a" tag]

    -- Get links from the database
    oldLinks <- getAllLinks

    -- Filter out old links
    let newLinks = filter (`notElem` oldLinks) links

    -- Save new links in the database and run callback
    forM_ newLinks $ \link -> do
      let siteName = viaNonEmpty head sites -- Safe head operation
      case siteName of
        Nothing -> putStrLn "No site provided."
        Just site -> do
          siteId <- getNewsSiteId (fromString site)
          case siteId of
            Nothing -> putStrLn $ "Site not found in the database: " ++ site
            Just siteId' -> do
              _ <- insertLinkIfNew link siteId'
              handleNewArticle link

    -- Wait for a while before polling the same site again
    threadDelay (60 * 60 * 1000000) -- 1 hour

runInfinitely :: IO () -> IO ()
runInfinitely action = action >> runInfinitely action

-- pollNewsSite function and related parts have been commented out as they're currently not connected with the rest of the code.
-- If NewsSite is defined in another part of your code, you can uncomment these.

-- import Database.Database (NewsSite, newsSiteName) -- this is if NewsSite is defined in Database.Database

{- 
pollNewsSite :: NewsSite -> IO ()
pollNewsSite site = do
  let siteName = newsSiteName site
  links <- scrapeNewsSite site
  forM_ links $ \link -> do
    maybeExistingArticle <- runDB $ getBy $ UniqueArticleLink link
    when (isNothing maybeExistingArticle) $ do
      -- if the article doesn't exist in the database, insert it
      runDB $ insert $ Article link siteName
      -- then run the callback function
      callback link
  -- sleep for a bit
  threadDelay $ 60 * 1000000 -- 60 seconds
  -- then start again
  pollNewsSite site
-}