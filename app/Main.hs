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
  let baseQuery = readStr $ head args
  let scowlSize = fromInt $ read (args !! 1)
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
  filteredResults <- filterResults baseQuery results scowlSize
  _ <- writeFilteredWordsToFile baseQuery filteredResults
  print $ show ((length . join) filteredResults) ++ " filtered results"
  -- find and record exceptional results
  let exceptionalResults =
        findExceptionalResults baseQuery resultPairs (join filteredResults)
  _ <- writeExceptionalWordsToFile baseQuery exceptionalResults
  print $ show (length exceptionalResults) ++ " exceptional results"
