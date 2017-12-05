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
  _ <- runReaderT createQueriesTable config
  _ <- runReaderT createResultsTable config
  result <- runReaderT (recursiveInstasearch $ query ++ " ") config
  close connection
  print $ show (length result) ++ " results recorded"
