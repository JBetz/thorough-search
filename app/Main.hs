module Main where

import Config
import Control.Monad.Reader
import Database.SQLite.Simple (close, open)
import Filter
import Instasearch
import Model
import Storage
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- read command line arguments
  let baseQuery = fromString $ head args
  conn <- open connStr
  let config = Config baseQuery conn
  -- run search
  printEvent "[START] Search"
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch baseQuery 1
  searchResults <- runReaderT actions config
  printStats $ show searchResults ++ " search results recorded"
  printEvent "[END] Search"
  -- get results from database
  printEvent "[START] Filter" 
  resultPairs <- runReaderT selectUniqueResults config
  let results = fmap snd resultPairs
  close conn
  printStats $ show (length results) ++ " total results"
  -- filter and record scowl results
  filteredResults <- filterResults baseQuery 4 results
  printStats $ show (length $ concatMap _results filteredResults) ++ " filtered results"
  _ <- writeFilteredWordsToFile baseQuery filteredResults
  -- find and record exceptional results
  let exceptionalResults = findExceptionalResults baseQuery resultPairs filteredResults
  printStats $ show (length exceptionalResults) ++ " exceptional results"
  _ <- writeExceptionalWordsToFile baseQuery exceptionalResults
  printEvent "[END] Filter"