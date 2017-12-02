{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Monad.Logger    (runNoLoggingT)
import           Control.Monad.Reader
import           Data.List               (sort)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (createSqlitePool, runMigration,
                                          runSqlPool)
import           Instasearch
import           Persistence
import           Scowl
import           System.Environment      (getArgs)

main :: IO ()
main = do
  args <- getArgs
  connectionPool <- runNoLoggingT $ createSqlitePool connStr 2
  runNoLoggingT $ runSqlPool (runMigration migrateAll) connectionPool
  let baseQuery = head args
  let scowlSize = read $ args !! 1
  wordList <- loadWordsFromScowl scowlSize
  let config = Config baseQuery wordList connectionPool
  result <- runReaderT (recursiveInstasearch $ baseQuery ++ " ") config
  print $ show (length result) ++ " results recorded"

connStr :: Text
connStr = "file:./output/autocomplete.db"
