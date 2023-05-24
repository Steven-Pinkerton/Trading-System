{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use alternative" #-}

module Database.Database where

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (exponentialBackoff, retrying)
import Database.Persist.Postgresql (
  BackendKey (SqlBackendKey),
  Entity (entityKey, entityVal),
  PersistStoreWrite (insert),
  PersistUniqueRead (getBy),
  SqlPersistT,
  selectList,
  withPostgresqlPool, runSqlPool
 )
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import Prelude hiding (encodeUtf8)
import qualified Data.Text.Encoding as Encoding
import Control.Monad.Trans.Resource (runResourceT)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
NewsSite
    name Text
    UniqueName name
    deriving Show Eq

Article
  title Text
  link Text
  newsSiteId NewsSiteId
  UniqueLink link
  deriving Show Eq
|]

connStr :: Text
connStr = "host=localhost dbname=test user=test password=test port=5432"

runDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB query = runResourceT $
  runNoLoggingT $
    withPostgresqlPool (Encoding.encodeUtf8 connStr) 10 $ \pool -> runSqlPool query pool

insertLinkIfNew :: Text -> NewsSiteId -> IO Bool
insertLinkIfNew url newsSiteId = runDB $ do
  mArticle <- getBy $ UniqueLink url
  case mArticle of
    Nothing -> do
      -- The article is new
      _ <- insert $ Article "<title>" url newsSiteId
      return True
    Just _ -> return False -- The article already exists

getNewsSiteId :: Text -> IO (Maybe NewsSiteId)
getNewsSiteId siteName = runDB $ do
  mNewsSite <- getBy $ UniqueName siteName
  return $ fmap entityKey mNewsSite

getNewsSiteIdWithRetry :: Text -> IO (Maybe NewsSiteId)
getNewsSiteIdWithRetry siteName = retrying (exponentialBackoff 500000) shouldRetry $ \_ -> getNewsSiteId siteName
  where
    shouldRetry _ maybeId = return $ isNothing maybeId

gamesIndustryId :: IO NewsSiteId
gamesIndustryId = do
  mId <- getNewsSiteIdWithRetry "gamesindustry"
  case mId of
    Just id' -> return id'
    Nothing -> error "Unable to find gamesindustry in the NewsSite table."

gamesutraId :: IO NewsSiteId
gamesutraId = do
  mId <- getNewsSiteIdWithRetry "gamasutra"
  case mId of
    Just id' -> return id'
    Nothing -> error "Unable to find gamasutra in the NewsSite table."

getAllLinks :: IO [Text]
getAllLinks = runDB $ do
  articles <- selectList [] []
  return $ map (articleLink . entityVal) articles