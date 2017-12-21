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
  , selectAllResults
  , selectAllResultPairs
  , selectQueryResults
  , ranQuery
  ) where

import           Config
import           Control.Monad.Reader
import           Data.Map               (Map, fromList, assocs)
import           Data.String            (fromString)
import           Data.Text              (Text, unpack)
import           Data.Tuple             (swap)
import           Database.SQLite.Simple

data QueriesField = QueriesField Int Text
  deriving (Show)

data ResultsField = ResultsField Int Text Text Text
  deriving (Show)

instance FromRow ResultsField where
  fromRow = ResultsField <$> field <*> field <*> field <*> field

instance ToRow ResultsField where
  toRow (ResultsField id_ bq eq value) = toRow (id_, bq, eq, value)

instance FromRow QueriesField where
  fromRow = QueriesField <$> field <*> field

connStr :: String
connStr = "file:./output/autocomplete.db"

createQueriesTable :: App ()
createQueriesTable = do
  (Config bq conn) <- ask
  liftIO $
    execute_
      conn
      (fromString $
       "CREATE TABLE IF NOT EXISTS " ++
       bq ++ "_queries (id INTEGER PRIMARY KEY, value TEXT UNIQUE)")

createResultsTable :: App ()
createResultsTable = do
  (Config bq conn) <- ask
  liftIO $
    execute_
      conn
      (fromString $
       "CREATE TABLE IF NOT EXISTS " ++
       bq ++
       "_results (id INTEGER PRIMARY KEY, base_query TEXT, expanded_query TEXT, value TEXT UNIQUE)")

insertResultList :: (String, [String]) -> App [()]
insertResultList result = do
  _ <- insertQuery (fst result)
  traverse (insertResult (fst result)) (snd result)

insertQuery :: String -> App ()
insertQuery queryString = do
  (Config bq conn) <- ask
  liftIO $
    execute
      conn
      (fromString $
       "INSERT OR IGNORE INTO " ++ bq ++ "_queries (value) VALUES (?)")
      [queryString]

insertResult :: String -> String -> App ()
insertResult expandedQuery result = do
  (Config bq conn) <- ask
  liftIO $
    execute
      conn
      (fromString $
       "INSERT OR IGNORE INTO " ++
       bq ++ "_results (base_query, expanded_query, value) VALUES (?, ?, ?)")
      (bq, expandedQuery, result)

selectAllResultPairs :: App [(String, String)]
selectAllResultPairs = do
  totalResults <- selectAllResults
  pure $ fmap (\(ResultsField _ _ eq v) -> (unpack eq, unpack v)) totalResults

selectAllResults :: App [ResultsField]
selectAllResults = do
  (Config bq conn) <- ask
  liftIO $
    query_
      conn
      (fromString $ "SELECT * FROM " ++ bq ++ "_results")

ranQuery :: String -> App Bool
ranQuery eq = do
  (Config bq conn) <- ask
  liftIO $ do
    res <-
      query
        conn
        (fromString $ "SELECT * FROM " ++ bq ++ "_queries WHERE value = ?")
        [eq] :: IO [QueriesField]
    pure $ not (null res)

selectQueryResults :: String -> App (String, [String])
selectQueryResults q = do
  (Config bq conn) <- ask
  liftIO $ do
    res <-
      query
        conn
        (fromString $ "SELECT * FROM " ++ bq ++ "_results WHERE expanded_query = ?")
        [q] :: IO [ResultsField]
    pure (q, fmap (\(ResultsField _ _ _ v) -> unpack v) res)
