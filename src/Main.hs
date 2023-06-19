{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'forever' from Relude" #-}
module Main where

import Control.Concurrent (threadDelay)
import Database.Database (migrateAll, runDB)
import Polling.Polling (startPolling)

main :: IO ()
main = do
  putStrLn "Starting up..."

  -- Initialize and migrate your database
  putStrLn "Initializing database..."
  runDB migrateAll

  -- Define your sites and urls
  let sites = ["site1", "site2", "site3"]
  let urls = ["url1", "url2", "url3"]

  forever $ do
    -- Start polling for new articles
    putStrLn "Polling sites for new articles..."
    startPolling sites urls

    -- Wait for a certain amount of time before polling again. The argument is in microseconds.
    -- For example, to wait for an hour you would use:
    -- threadDelay (60 * 60 * 1000000)

    threadDelay (60 * 60 * 1000000) -- adjust the delay as needed
  putStrLn "All done!"