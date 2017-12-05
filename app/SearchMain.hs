{-# LANGUAGE OverloadedStrings #-}

module SearchMain where

import           Config
import           Control.Monad.Reader
import           Database.SQLite.Simple (open, close)
import           Instasearch
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let query = head args
  connection <- open connStr
  let config = Config query connection
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch (query ++ " ")
  result <- runReaderT actions config
  close connection
  print $ show (length result) ++ " results recorded"
