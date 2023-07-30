module Main where

import           Prelude                hiding (filter)

import           Config
import           Control.Monad.Reader
import           Database.SQLite.Simple (close, open)
import           Filter
import           Model
import           Search
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do

  -- configure
  args <- getArgs
  let bq = fromString $ head args
  configStr <- readFile "config.ini"
  let config =
        case runConfigParser configStr of
          Right cfg -> cfg
          Left str  -> error str
  conn <- open $ databasePath config
  createQueriesTable bq conn
  createResultsTable bq conn

  -- search
  printEvent "SEARCH"
  searchResultCount <- runReaderT (thoroughSearch bq conn 1) (config_search config)
  printStats $ show searchResultCount ++ " total results"

  -- filter
  printEvent "FILTER"
  results <- allResults bq conn
  filteredResults <- runReaderT (filter bq results) (config_filter config)
  close conn
  printStats $ show (length results) ++ " unique results"
  printStats $ show (length filteredResults) ++ " filtered results"

  -- sort
  printEvent "SORT"
  let sortedResults = sort filteredResults

  -- record
  printEvent "RECORD"
  _ <- commit bq sortedResults
  pure ()
