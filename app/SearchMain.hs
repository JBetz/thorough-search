{-# LANGUAGE OverloadedStrings #-}

module SearchMain where

import           Config
import           Control.Monad.Reader
import           Database.SQLite.Simple (close, open)
import           Instasearch
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let query = head args
  conn <- open connStr
  let config = Config query conn
  let actions = do
        createQueriesTable
        createResultsTable
        recursiveInstasearch (query ++ " ") 3
  result <- runReaderT actions config
  close conn
  print $ show (length result) ++ " results recorded"
