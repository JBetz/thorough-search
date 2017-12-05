{-# LANGUAGE OverloadedStrings #-}

module FilterMain where

import           Config
import           Control.Monad.Reader
import           Data.Text              (unpack)
import           Database.SQLite.Simple (close, open)
import           Scowl
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  connection <- open connStr
  let query = head args
  let scowlSize = read $ args !! 1
  let config = Config query connection
  wordLists <- loadWordsFromScowl (fromInt scowlSize)
  dbResults <- runReaderT selectResults config
  close connection
  print $ show (length dbResults) ++ " total results"
  let results = fmap (\(ResultsField _ _ _ v) -> unpack v) dbResults
  let filteredResults = fmap (filterResults query results) wordLists
  output <- traverse (writeWordsToFile query) filteredResults
  print $ show (length output) ++ " filtered results"
