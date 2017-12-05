{-# LANGUAGE OverloadedStrings #-}

module SearchMain where

import           Config
import           Control.Monad.Logger   (runNoLoggingT)
import           Control.Monad.Reader
import           Database.SQLite.Simple
import           Instasearch
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let baseQuery = head args
  connection <- open connStr
  let config = Config baseQuery connection
  runReaderT createQueriesTable config
  runReaderT createResultsTable config
  result <- runReaderT (recursiveInstasearch $ baseQuery ++ " ") config
  close connection
  print $ show (length result) ++ " results recorded"
