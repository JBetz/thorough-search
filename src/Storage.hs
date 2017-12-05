{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Storage
  ( ResultsField(..)
  , createQueriesTable
  , createResultsTable
  , connStr
  , insertResultList
  , selectResults
  , ranQuery
  ) where

import           Config
import           Control.Monad.Reader
import           Data.Text              (Text)
import           Database.SQLite.Simple

data QueriesField =
  QueriesField Int
               Text
  deriving (Show)

data ResultsField =
  ResultsField Int
               Text
               Text
               Text
  deriving (Show)

instance FromRow ResultsField where
  fromRow = ResultsField <$> field <*> field <*> field <*> field

instance ToRow ResultsField where
  toRow (ResultsField id bq eq value) = toRow (id, bq, eq, value)

instance FromRow QueriesField where
  fromRow = QueriesField <$> field <*> field

connStr :: String
connStr = "file:./output/autocomplete.db"

createQueriesTable :: App (IO ())
createQueriesTable = do
  (Config bq conn) <- ask
  pure $
    execute
      conn
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY, value TEXT)"
      [bq ++ "_queries"]

createResultsTable :: App (IO ())
createResultsTable = do
  (Config bq conn) <- ask
  pure $
    execute
      conn
      "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY, base_query TEXT, expanded_query TEXT, value TEXT)"
      [bq ++ "_results"]

insertResultList :: (String, [String]) -> App [()]
insertResultList result = do
  insertQuery (fst result)
  traverse (insertResult (fst result)) (snd result)

insertQuery :: String -> App (IO ())
insertQuery query = do
  (Config bq conn) <- ask
  pure $
    execute conn "INSERT INTO ? (value) VALUES (?)" (bq ++ "_queries", query)

insertResult :: String -> String -> App ()
insertResult expandedQuery result = do
  (Config bq conn) <- ask
  liftIO $
    execute
      conn
      "INSERT INFO ? (base_query, expanded_query, value) VALUES (?, ?, ?)"
      (bq, expandedQuery, result)

selectResults :: App [ResultsField]
selectResults = do
  (Config bq conn) <- ask
  liftIO $
    query conn "SELECT * FROM ? WHERE base_query = ?" (bq ++ "_results", bq)

ranQuery :: String -> App Bool
ranQuery q = do
  (Config bq conn) <- ask
  liftIO $ do
    res <-
      query conn "SELECT * FROM ? WHERE value = ?" (bq ++ "_queries", q) :: IO [QueriesField]
    pure $ not (null res)
