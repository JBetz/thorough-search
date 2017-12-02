{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Persistence
  ( Result
  , Query
  , insertResult
  , selectResults
  , migrateAll
  , ranQuery
  , resultValue
  ) where

import           Config
import           Control.Monad.Reader
import           Data.Pool
import           Database.Persist
import           Database.Persist.Sql    (Key, SqlBackend, count, insertUnique,
                                          runSqlPool, (==.))
import           Database.Persist.Sqlite
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Result
    baseQuery String
    value String
    UniqueResult baseQuery value
    deriving Show
Query
    value String
    UniqueQuery value
    deriving Show
|]

insertResult :: (String, [String]) -> App [Maybe (Key Result)]
insertResult result = do
  config <- ask
  let db = connectionPool config
  let bq = baseQuery config
  runSqlPool (insertUnique (Query (fst result))) db
  traverse (\r -> runSqlPool (insertUnique (Result bq r)) db) (snd result)

selectResults :: App [Result]
selectResults = do
  config <- ask
  let db = connectionPool config
  let bq = baseQuery config
  entityResults <-
    runSqlPool (selectList [ResultBaseQuery ==. bq] []) db
  pure $ fmap entityVal entityResults

ranQuery :: String -> App Bool
ranQuery query = do
  config <- ask
  let db = connectionPool config
  resultCount <- runSqlPool (count [QueryValue ==. query]) db
  pure $ resultCount > 0
