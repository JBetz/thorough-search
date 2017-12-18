module Config
  ( App
  , Config(..)
  ) where

import           Control.Monad.Reader
import           Database.SQLite.Simple

type App = ReaderT Config IO

data Config = Config
  { baseQuery  :: String
  , connection :: Connection
  }
