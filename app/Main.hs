{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad.Reader
import Database.SQLite.Simple (close, open)
import Filter
import Instasearch
import Model
import Storage
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- read command line arguments
  let baseQuery = fromString $ head args
  conn <- open connStr
  let config = Config baseQuery conn
  -- run search
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch baseQuery 1
  searchResults <- runReaderT actions config
  print $ show searchResults ++ " search results recorded"
  -- get results from database
  resultPairs <- runReaderT selectAllResultPairs config
  let results = fmap snd resultPairs
  close conn
  print $ show (length results) ++ " total results"
  -- filter and record scowl results
  _ <- createDirectoryIfMissing False ("./output/" ++ show_ baseQuery)
  filteredResults <- filterResults baseQuery 4 results
  print $ show (length $ fmap _results filteredResults) ++ " filtered results"
  _ <- writeFilteredWordsToFile baseQuery filteredResults
  -- find and record exceptional results
  let exceptionalResults =
        findExceptionalResults baseQuery resultPairs filteredResults
  print $ show (length exceptionalResults) ++ " exceptional results"
  _ <- writeExceptionalWordsToFile baseQuery exceptionalResults
  pure ()