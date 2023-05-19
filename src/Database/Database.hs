{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}


module Database.Database where

import Control.Monad.Logger (runStderrLoggingT, NoLoggingT)
import Database.Persist.Postgresql
    ( BackendKey(SqlBackendKey),
      runSqlPersistMPool,
      SqlPersistT,
      withPostgresqlPool,
      PersistUniqueRead(getBy),
      PersistStoreWrite(insert),
      runSqlPersistMPool,
      withPostgresqlPool, Entity (entityKey, entityVal), selectList )
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
import Control.Monad.Trans.Resource ( ResourceT )

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

connStr = "host=localhost dbname=test user=test password=test port=5432"

runDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB query = runStderrLoggingT $
  withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ runSqlPersistMPool query pool

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

gamesIndustryId :: IO NewsSiteId
gamesIndustryId = do
  mId <- getNewsSiteId "gamesindustry"
  case mId of
    Just id' -> return id'
    Nothing -> error "Unable to find gamesindustry in the NewsSite table."

gamesutraId :: IO NewsSiteId
gamesutraId = do
  mId <- getNewsSiteId "gamasutra"
  case mId of
    Just id' -> return id'
    Nothing -> error "Unable to find gamasutra in the NewsSite table."


getAllLinks :: IO [Text]
getAllLinks = runDB $ do
  articles <- selectList [] []
  return $ map (articleLink . entityVal) articles