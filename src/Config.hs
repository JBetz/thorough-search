module Config
  ( App
  , Config(..)
  ) where

import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import Model

type App = ReaderT Config IO

data Config = Config
  { query :: Query
  , connection :: Connection
  }
