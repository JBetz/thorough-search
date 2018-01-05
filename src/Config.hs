{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , SearchConfig(..)
  , FilterConfig(..)
  , databasePath
  , searchConfig
  , filterConfig
  , emailConfig
  , runConfigParser
  , printEvent
  , printStats
  , printInfo
  ) where

import Control.Lens.TH
import Data.Ini.Config
import Data.Text

data Config = Config
  { _databasePath :: FilePath
  , _searchConfig :: SearchConfig
  , _filterConfig :: FilterConfig
  , _emailConfig :: EmailConfig
  }

data SearchConfig = SearchConfig  
  { _maxQueryLength :: Int
  , _instasearchDelay :: Int
  , _retryDelay :: Int
  }

data FilterConfig = FilterConfig 
  { _scowlWordSets :: [String] }

data EmailConfig = EmailConfig   
  { _enable :: Bool
  , _from :: Maybe String
  , _to :: Maybe [String]
  , _host :: Maybe String
  }

makeLenses ''Config

runConfigParser :: String -> Either String Config
runConfigParser str =
  parseIniFile (pack str) configParser

configParser :: IniParser Config
configParser = do
  dbPath <- section "DATABASE" $ do
    fieldOf "connectionString" string
  searchCfg <- section "SEARCH" $ do
    mql <- fieldOf "maxQueryLength" number
    isd <- fieldOf "instasearchDelay" number
    rd <- fieldOf "retryDelay" number
    pure $ SearchConfig mql isd rd
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