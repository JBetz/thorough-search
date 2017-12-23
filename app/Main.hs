{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad.Reader
import Database.SQLite.Simple (close, open)
import Instasearch
import Scowl
import Storage
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

main :: IO ()
main
 = do
  args <- getArgs
  -- read command line arguments
  let query = head args
  let scowlSize = fromInt $ read (args !! 1)
  -- initialize configuration data
  conn <- open connStr
  let config = Config query conn
  -- run search
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch (query ++ " ") 1
  searchResults <- runReaderT actions config
  print $ show searchResults ++ " search results recorded"
  -- get results from database
  totalResults <- runReaderT selectAllResultPairs config
  close conn
  print $ show (length totalResults) ++ " total results"
  -- run scowl filter on results
  _ <- createDirectoryIfMissing False ("./output/" ++ query)
  filteredResults <- filterResults query totalResults scowlSize
  _ <- writeFilteredWordsToFile query filteredResults
  print $ show ((length . join) filteredResults) ++ " filtered results"
  -- find highly relevant words that were excluded by scowl
  let exceptionalResults =
        findExceptionalResults query totalResults (join filteredResults)
  _ <- writeExceptionalWordsToFile query exceptionalResults
  print $ show (length exceptionalResults) ++ " exceptional results"
