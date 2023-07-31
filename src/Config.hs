{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , SearchConfig(..)
  , FilterConfig(..)
  , runConfigParser
  ) where

import Data.Ini.Config
import Data.Text

data Config = Config
  { config_databasePath :: FilePath
  , config_search :: SearchConfig
  , config_filter :: FilterConfig
  }

data SearchConfig = SearchConfig
  { search_maxQueryCount :: Int
  , search_instasearchDelay :: Int
  , search_retryDelay :: Int
  }

data FilterConfig = FilterConfig
  { filter_scowlFilePath :: FilePath
  , filter_scowlWordSets :: [String] 
  }

runConfigParser :: String -> Either String Config
runConfigParser str =
  parseIniFile (pack str) configParser

configParser :: IniParser Config
configParser = do
  dbPath <- section "DATABASE" $ do
    fieldOf "connectionString" string
  searchCfg <- section "SEARCH" $ do
    mr <- fieldOf "maxQueryCount" number
    isd <- fieldOf "instasearchDelay" number
    rd <- fieldOf "retryDelay" number
    pure $ SearchConfig mr isd rd
  filterCfg <- section "FILTER" $ do
    sfp <- fieldOf "scowlFilePath" string
    sws <- fieldOf "scowlWordSets" (listWithSeparator "," string)
    pure $ FilterConfig sfp sws
  pure $ Config dbPath searchCfg filterCfg
