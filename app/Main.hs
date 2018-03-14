module Main where

import Config
import Control.Lens
import Database.SQLite.Simple (open, close)
import Filter as F
import Model
import Search
import Storage
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- configure
  let bq = fromString $ head args
  configStr <- readFile "config.ini"
  let config = 
        case runConfigParser configStr of
          Right cfg -> cfg
          Left str -> error str
  conn <- open $ view databasePath config
  createQueriesTable bq conn
  createResultsTable bq conn
  -- search
  printEvent "SEARCH"
  searchResultCount <- thoroughSearch bq conn 1 (view searchConfig config)
  printStats $ show searchResultCount ++ " search results recorded"
  -- filter
  printEvent "FILTER" 
  results <- allResults bq conn
  filteredResults <- F.filter bq results (view filterConfig config)
  close conn
  printStats $ show (length results) ++ " total results"
  printStats $ show (length filteredResults) ++ " filtered results"
  -- sort 
  printEvent "SORT"
  let sortedResults = F.sort filteredResults
  -- record
  printEvent "RECORD"
  _ <- record bq sortedResults
  pure ()