{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
  ( Config(..)
  , SearchConfig(..)
  , FilterConfig(..)
  , runConfigParser
  , printEvent
  , printStats
  , printInfo
  , msThreadDelay
  , secondsThreadDelay
  ) where

import           Control.Concurrent (threadDelay)
import           Data.Ini.Config
import           Data.Text

data Config = Config
  { databasePath :: FilePath
  , searchConfig :: SearchConfig
  , filterConfig :: FilterConfig
  , emailConfig  :: EmailConfig
  }

data SearchConfig = SearchConfig
  { maxRuntime       :: Int
  , instasearchDelay :: Int
  , retryDelay       :: Int
  }

data FilterConfig = FilterConfig
  { scowlWordSets :: [String] }

data EmailConfig = EmailConfig
  { enable :: Bool
  , from   :: Maybe String
  , to     :: Maybe [String]
  , host   :: Maybe String
  }

runConfigParser :: String -> Either String Config
runConfigParser str =
  parseIniFile (pack str) configParser

configParser :: IniParser Config
configParser = do
  dbPath <- section "DATABASE" $ do
    fieldOf "connectionString" string
  searchCfg <- section "SEARCH" $ do
    mr <- fieldOf "maxRuntime" number
    isd <- fieldOf "instasearchDelay" number
    rd <- fieldOf "retryDelay" number
    pure $ SearchConfig mr isd rd
  filterCfg <- section "FILTER" $ do
    sws <- fieldOf "scowlWordSets" (listWithSeparator "," string)
    pure $ FilterConfig sws
  emailCfg <- section "EMAIL" $ do
    e <- fieldOf "enable" flag
    f <- fieldMbOf "from" string
    t <- fieldMbOf "to" (listWithSeparator "," string)
    h <- fieldMbOf "host" string
    pure $ EmailConfig e f t h
  pure $ Config dbPath searchCfg filterCfg emailCfg

-- SYSTEM
printEvent :: String -> IO ()
printEvent str =
  putStrLn $ "\n" ++ str ++ "\n"

printStats :: String -> IO ()
printStats str =
  putStrLn $ " - " ++ str

printInfo :: String -> IO ()
printInfo str =
  putStrLn str

msThreadDelay :: Int -> IO ()
msThreadDelay ms =
  threadDelay $ ms * 1000

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds =
  threadDelay $ seconds * 1000 * 1000
