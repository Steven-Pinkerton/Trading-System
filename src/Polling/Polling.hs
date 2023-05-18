module Polling (startPolling) where

import Control.Concurrent
import Control.Monad
import Database.Database (getLinks, saveLink)
import Network.HTTP.Simple
import Text.HTML.TagSoup
import ArticleHandler (handleNewArticle)

startPolling :: Connection -> [String] -> (String -> IO ()) -> IO ()
startPolling conn urls callback = forM_ urls $ \url -> forkIO $
  infinitely $ do
    response <- httpBS (fromString url)
    let tags = parseTags (getResponseBody response)
        links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

    -- Get links from the database
    oldLinks <- getLinks conn

    -- Filter out old links
    let newLinks = filter (`notElem` oldLinks) links

    -- Save new links in the database
    forM_ newLinks $ \link -> do
      saveLink conn link
      callback link

    -- Wait for a while before polling the same site again
    threadDelay (60 * 60 * 1000000) -- 1 hour


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