{-# LANGUAGE OverloadedStrings #-}

module SearchMain where

import           Config
import           Control.Monad.Logger    (runNoLoggingT)
import           Control.Monad.Reader
import           Database.Persist.Sqlite (createSqlitePool, runMigration,
                                          runSqlPool)
import           Instasearch
import           Storage
import           System.Environment      (getArgs)

main :: IO ()
main = do
  args <- getArgs
  connectionPool <- runNoLoggingT $ createSqlitePool connStr 2
  runNoLoggingT $ runSqlPool (runMigration migrateAll) connectionPool
  let baseQuery = head args
  let config = Config baseQuery connectionPool
  result <- runReaderT (recursiveInstasearch $ baseQuery ++ " ") config
  print $ show (length result) ++ " results recorded"
