module Polling (startPolling) where

import ArticleHandler (handleNewArticle)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (catch)
import Database.Database (getNewsSiteId, insertLinkIfNew, linkExists)
import Network.HTTP.Simple (HttpException, Response, getResponseBody, httpBS, parseRequest)
import Text.HTML.TagSoup (fromAttrib, isTagOpenName, parseTags)

-- Define exception handler for network errors
networkHandler :: HttpException -> IO (Maybe (Response ByteString))
networkHandler e = do
  putStrLn $ "Network error: " ++ show e
  return Nothing

-- Define generic exception handler for all other errors
anyExceptionHandler :: SomeException -> IO ()
anyExceptionHandler e = putStrLn $ "An error occurred: " ++ show e

startPolling :: [Text] -> [Text] -> IO ()
startPolling sites urls = forConcurrently_ urls $ \url -> runInfinitely $ do
  request <- parseRequest (toString url)
  maybeResponse <- (Just <$> httpBS request) `catch` networkHandler
  case maybeResponse of
    Nothing -> putStrLn "Failed to get response."
    Just response -> do
      let tags = parseTags (getResponseBody response)
          links =
            [ toText . toString $ (decodeUtf8 @Text $ fromAttrib "href" tag)
            | tag <- tags
            , isTagOpenName "a" tag
            ]

      -- Check in the database whether each link already exists and filter out those that do
      newLinks <- filterM (fmap not . linkExists) links

      -- Save new links in the database and run callback
      forM_ newLinks $ \link -> do
        let siteName = viaNonEmpty head sites -- Safe head operation
        case siteName of
          Nothing -> putStrLn "No site provided."
          Just site -> do
            siteId <- getNewsSiteId site
            case siteId of
              Nothing -> putStrLn $ "Site not found in the database: " ++ toString site
              Just siteId' -> do
                _ <- insertLinkIfNew link siteId'
                handleNewArticle link

  -- Wait for a while before polling the same site again
  threadDelay (60 * 60 * 1000000) -- 1 hour

-- Define a function that runs an action indefinitely, with our generic exception handler
runInfinitely :: IO () -> IO ()
runInfinitely action = (action `catch` anyExceptionHandler) >> runInfinitely action

-- Remember to handle potential exceptions in 'linkExists' and 'insertLinkIfNew'
-- Also remember to replace 'String' with 'Text' in your database functions. action

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