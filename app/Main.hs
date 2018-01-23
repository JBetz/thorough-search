module Main where

import Config
import Control.Lens
import Database.SQLite.Simple (open, close)
import Filter
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
  printEvent "[START] Search"
  searchResultCount <- thoroughSearch bq conn 1 (view searchConfig config)
  printStats $ show searchResultCount ++ " search results recorded"
  printEvent "[END] Search"
  -- filter
  printEvent "[START] Filter" 
  resultPairs <- selectUniqueResults bq conn
  let results = fmap snd resultPairs
  let matchingResults = filter (matches bq) results
  filteredResults <- filterResults bq matchingResults (view filterConfig config)
  close conn
  printStats $ show (length results) ++ " total results"
  printStats $ show (length matchingResults) ++ " matching results"
  printStats $ show (length $ concatMap _results filteredResults) ++ " filtered results"
  printEvent "[END] Filter"
  -- record
  printEvent "[START] Record"
  _ <- writeFilteredWordsToFile bq filteredResults
  printEvent "[END] Record"
