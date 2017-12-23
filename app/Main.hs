{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad.Reader
import Database.SQLite.Simple (close, open)
import Instasearch
import Model
import Scowl
import Storage
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- read command line arguments
  let query = readStr $ head args
  let scowlSize = fromInt $ read (args !! 1)
  conn <- open connStr
  let config = Config query conn
  -- run search
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch query 1
  searchResults <- runReaderT actions config
  print $ show searchResults ++ " search results recorded"
  -- get results from database
  totalResults <- runReaderT selectAllResultPairs config
  close conn
  print $ show (length totalResults) ++ " total results"
  -- filter and record scowl results
  _ <- createDirectoryIfMissing False ("./output/" ++ show_ query)
  filteredResults <- filterResults query totalResults scowlSize
  _ <- writeFilteredWordsToFile query filteredResults
  print $ show ((length . join) filteredResults) ++ " filtered results"
  -- find and record exceptional results
  let exceptionalResults =
        findExceptionalResults query totalResults (join filteredResults)
  _ <- writeExceptionalWordsToFile query exceptionalResults
  print $ show (length exceptionalResults) ++ " exceptional results"
