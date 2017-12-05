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
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Types

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
    execute_
      conn
      (fromString $ "CREATE TABLE IF NOT EXISTS " ++ bq ++ "_queries (id INTEGER PRIMARY KEY, value TEXT)")

createResultsTable :: App (IO ())
createResultsTable = do
  (Config bq conn) <- ask
  pure $
    execute_
      conn
      (fromString $ "CREATE TABLE IF NOT EXISTS " ++ bq ++ "_results (id INTEGER PRIMARY KEY, base_query TEXT, expanded_query TEXT, value TEXT)")

insertResultList :: (String, [String]) -> App [()]
insertResultList result = do
  insertQuery (fst result)
  traverse (insertResult (fst result)) (snd result)

insertQuery :: String -> App (IO ())
insertQuery query = do
  (Config bq conn) <- ask
  pure $
    execute
      conn
      (fromString $ "INSERT INTO " ++ bq ++ "_queries (value) VALUES (?)")
      [query]

insertResult :: String -> String -> App ()
insertResult expandedQuery result = do
  (Config bq conn) <- ask
  liftIO $
    execute
      conn
      (fromString $
       "INSERT INTO " ++
       bq ++ "_results (base_query, expanded_query, value) VALUES (?, ?, ?)")
      (bq, expandedQuery, result)

selectResults :: App [ResultsField]
selectResults = do
  (Config bq conn) <- ask
  liftIO $
    query
      conn
      (fromString $ "SELECT * FROM " ++ bq ++ "_results WHERE base_query = ?")
      [bq]

ranQuery :: String -> App Bool
ranQuery q = do
  (Config bq conn) <- ask
  liftIO $ do
    res <-
      query conn "SELECT * FROM ? WHERE value = ?" (bq ++ "_queries", q) :: IO [QueriesField]
    pure $ not (null res)
