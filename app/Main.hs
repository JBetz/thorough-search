module Main where

import Config
import Control.Lens
import Database.SQLite.Simple (close, open)
import Filter
import Model
import Search
import Storage
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- read command line arguments
  let bq = fromString $ head args
  configStr <- readFile "config.ini"
  let config = case runConfigParser configStr of
        Right cfg -> cfg
        Left str -> error str
  conn <- open $ view databasePath config
  -- run search
  printEvent "[START] Search"
  createQueriesTable bq conn
  createResultsTable bq conn
  searchResultCount <- recursiveInstasearch bq conn (view filterConfig config, view searchConfig config)
  printStats $ show searchResultCount ++ " search results recorded"
  printEvent "[END] Search"
  -- get results from database
  printEvent "[START] Filter" 
  resultPairs <- selectUniqueResults bq conn
  let results = fmap snd resultPairs
  let matchingResults = filter (matches bq) results
  close conn
  printStats $ show (length results) ++ " total results"
  printStats $ show (length matchingResults) ++ " matching results"
  -- filter and record results
  filteredResults <- filterResults bq matchingResults (view filterConfig config)
  printStats $ show (length $ concatMap _results filteredResults) ++ " filtered results"
  _ <- writeFilteredWordsToFile bq filteredResults
  -- _ <- emailResults outputFile
  printEvent "[END] Filter"