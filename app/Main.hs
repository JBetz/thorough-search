{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Monad.Reader
import           Database.SQLite.Simple (close, open)
import           Instasearch
import           Scowl
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  -- parse command line arguments
  args <- getArgs
  let query = head args
  let scowlSize = fromInt $ read (args !! 1)
  conn <- open connStr
  -- initialize configuration variables
  let config = Config query conn
  -- run search
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch (query ++ " ") 3
  searchResults <- runReaderT actions config
  print $ show (length searchResults) ++ " search results recorded"
  -- get results from database
  uniqueResults <- runReaderT selectUniqueResults config
  close conn
  print $ show (length uniqueResults) ++ " unique results"
  -- run scowl filter on results
  filteredResults <- filterResults query uniqueResults scowlSize
  _ <- writeFilteredWordsToFile query filteredResults
  print $ show ((length . join) filteredResults) ++ " filtered results"
  -- find highly relevant words that were excluded by scowl
  let exceptionalResults = findExceptionalResults query uniqueResults (join filteredResults)
  _ <- writeExceptionalWordsToFile query exceptionalResults
  print $ show (length exceptionalResults) ++ " exceptional results"
