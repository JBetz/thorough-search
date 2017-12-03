{-# LANGUAGE OverloadedStrings #-}

module FilterMain where

import           Config
import           Control.Monad.Logger    (runNoLoggingT)
import           Control.Monad.Reader
import           Data.List               (isPrefixOf, sort, (\\))
import           Data.Text               (Text)
import           Database.Persist.Sqlite (createSqlitePool, runMigration,
                                          runSqlPool)
import           Debug.Trace
import           Instasearch
import           Scowl
import           Storage
import           System.Environment      (getArgs)
import           System.IO.Unsafe

main :: IO ()
main = do
  args <- getArgs
  connectionPool <- runNoLoggingT $ createSqlitePool connStr 2
  let baseQuery = head args
  let scowlSize = read $ args !! 1
  let config = Config baseQuery connectionPool
  wordLists <- loadWordsFromScowl (fromInt scowlSize)
  totalResults <- runReaderT selectResults config
  print $ show (length totalResults) ++ " total results"
  let extractedResults = fmap resultValue totalResults
  let filteredResults =
        fmap (filterResults baseQuery extractedResults extractedResults) wordLists
  output <-
    traverse (writeWordsToFile baseQuery) filteredResults
  print $ show (length output) ++ " filtered results"
