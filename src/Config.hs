module Config
  ( App
  , Config(..)
  , printEvent
  , printStats
  , printInfo
  ) where

import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import Model

type App = ReaderT Config IO

data Config = Config
  { query :: Query
  , connection :: Connection
  }

-- LOGGING
printEvent :: String -> IO ()
printEvent str =
  putStrLn $ "\n" ++ str ++ "\n"

printStats :: String -> IO ()
printStats str =
  putStrLn $ " - " ++ str

printInfo :: String -> IO ()
printInfo str =
  putStrLn str