{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Monad.Logger    (runNoLoggingT)
import           Control.Monad.Reader
import           Data.List               (sort, (\\), isPrefixOf)
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
  let maxWordCount = read $ args !! 2
  wordList <- loadWordsFromScowl scowlSize
  let config = Config baseQuery wordList connectionPool
  result <- runReaderT (recursiveInstasearch $ baseQuery ++ " ") config
  print $ show (length result) ++ " results recorded"
  totalResults <- runReaderT selectResults config
  print $ show (length totalResults) ++ " total results"
  let extractedResults = fmap resultValue totalResults
  let filteredResults =
        filter
          (\r ->
             let rWords = words r
             in length rWords <= maxWordCount &&
                (baseQuery `isPrefixOf` head rWords) && null (rWords \\ wordList))
          extractedResults
  print $ show (length filteredResults) ++ " filtered results"
  output <-
    traverse
      (\r -> appendFile (baseQuery ++ ".txt") (r ++ "\n"))
      (sort filteredResults)
  pure ()

connStr :: Text
connStr = "file:./output/autocomplete.db"
