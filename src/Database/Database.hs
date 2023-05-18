{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Database.Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text)
import Database.Persist.Postgresql
import Database.Persist.TH

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
    deriving Show Eq
|]

connStr = "host=localhost dbname=test user=test password=test port=5432"

runDB :: (MonadIO m) => SqlPersistT IO a -> m a
runDB query = liftIO $ runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ runSqlPersistMPool query pool