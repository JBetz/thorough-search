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
  , selectUniqueResults
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
insertResultList result =
  if (not . null . snd) result
    then do
      _ <- insertQuery (fst result)
      traverse (insertResult (fst result)) (snd result)
    else pure []

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

selectUniqueResults :: App (Map String String)
selectUniqueResults = do
  totalResults <- selectAllResults
  let resultAssocs = fmap (\(ResultsField _ _ eq v) -> (unpack v, unpack eq)) totalResults
  pure $ fromList (swap <$> assocs (fromList resultAssocs))

selectAllResults :: App [ResultsField]
selectAllResults = do
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
      query
        conn
        (fromString $ "SELECT * FROM " ++ bq ++ "_queries WHERE value = ?")
        [q] :: IO [QueriesField]
    pure $ not (null res)
