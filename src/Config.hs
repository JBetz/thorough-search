module Config
  ( App
  , Config(..)
  ) where

import           Control.Monad.Reader
import           Data.Pool
import           Database.Persist.Sql (SqlBackend)

type App = ReaderT Config IO

data Config = Config
  { baseQuery      :: String
  , connectionPool :: Pool SqlBackend
  }
