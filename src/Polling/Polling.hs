module Polling (startPolling) where

import ArticleHandler (handleNewArticle)
import Control.Concurrent (forkIO, threadDelay, Chan, writeList2Chan)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (catch)
import Database.Database (getNewsSiteId, insertLinkIfNew, runDB, EntityField (ArticleLink), Article (articleLink))
import Database.Esqueleto.Legacy
    ( (^.), from, in_, select, valList, where_, Entity(entityVal) )
import Network.HTTP.Simple (HttpException, Response, getResponseBody, httpBS, parseRequest)
import Text.HTML.TagSoup (fromAttrib, isTagOpenName, parseTags)
import Data.List ( (\\) )

-- Define your custom exception
data RateLimitingException = RateLimited deriving stock (Show, Typeable)

instance Exception RateLimitingException

-- Define your custom data type
data PollResult
  = PollRateLimited
  | Successful (Response ByteString)
  deriving stock (Show)

-- Define exception handler for network errors
networkHandler :: HttpException -> IO (Maybe (Response ByteString))
networkHandler e = do
  putStrLn $ "Network error: " ++ show e
  return Nothing

-- Define generic exception handler for all other errors
anyExceptionHandler :: SomeException -> IO ()
anyExceptionHandler e = putStrLn $ "An error occurred: " ++ show e

startPolling :: [Text] -> [Text] -> IO ()
startPolling sites urls = do
  -- Create a new queue
  queue <- newChan

  -- Fill the queue with URLs
  writeList2Chan queue urls

  -- Start a fixed number of workers
  replicateM_ 10 (forkIO (worker sites queue))

pollUrl :: Text -> IO PollResult
pollUrl url = do
  request <- parseRequest (toString url)
  catch (Successful <$> httpBS request) (\e -> networkHandler e >> pure PollRateLimited)

worker :: [Text] -> Chan Text -> IO ()
worker sites queue = runInfinitely $ do
  -- Take a URL from the queue
  url <- readChan queue

  -- Poll the URL and handle rate limiting
  result <- pollUrl url
  case result of
    PollRateLimited -> do
      -- If rate limited, delay and put the URL back in the queue
      threadDelay (60 * 60 * 1000000) -- 1 hour
      writeChan queue url
    Successful response -> handleResponse sites response

handleResponse :: [Text] -> Response ByteString -> IO ()
handleResponse sites response = do
  let tags = parseTags (getResponseBody response)
      links =
        [ toText . toString $ (decodeUtf8 @Text $ fromAttrib "href" tag)
        | tag <- tags
        , isTagOpenName "a" tag
        ]

  -- Get non-existing links
  newLinks <- runDB $ do
    existingLinks <- select $ from $ \link -> do
      where_ (link ^. ArticleLink `in_` valList links)
      return link
    return $ links \\ map (articleLink . entityVal) existingLinks

  -- Save new links in the database and run callback
  forM_ newLinks $ \link -> do
    let siteName = viaNonEmpty Prelude.head sites -- Safe head operation
    case siteName of
      Nothing -> putStrLn "No site provided."
      Just site -> do
        siteId <- getNewsSiteId site
        case siteId of
          Nothing -> putStrLn $ "Site not found in the database: " ++ toString site
          Just siteId' -> do
            _ <- insertLinkIfNew link siteId'
            handleNewArticle link

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