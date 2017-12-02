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
  , migrateAll
  , ranQuery
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
    value String
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
  runSqlPool (insertUnique (Query (fst result))) db
  traverse (\r -> runSqlPool (insertUnique (Result r)) db) (snd result)

ranQuery :: String -> App Bool
ranQuery query = do
  config <- ask
  let db = connectionPool config
  resultCount <- runSqlPool (count [QueryValue ==. query]) db
  pure $ resultCount > 0
