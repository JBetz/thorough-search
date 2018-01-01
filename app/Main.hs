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
  _ <- createQueriesTable bq conn
  _ <- createResultsTable bq conn
  searchResultCount <- recursiveInstasearch bq 1 conn (view filterConfig config, view searchConfig config)
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
  -- filter and record scowl results
  filteredResults <- filterResults bq 4 matchingResults (view filterConfig config)
  printStats $ show (length $ concatMap _results filteredResults) ++ " filtered results"
  _ <- writeFilteredWordsToFile bq filteredResults
  -- find and record exceptional results
  let exceptionalResults = findExceptionalResults bq resultPairs filteredResults
  printStats $ show (length exceptionalResults) ++ " exceptional results"
  _ <- writeExceptionalWordsToFile bq exceptionalResults
  let outputFile = "./output/" ++ show_ bq
  _ <- archiveResults outputFile
  -- _ <- emailResults outputFile
  printEvent "[END] Filter"