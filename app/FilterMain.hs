{-# LANGUAGE OverloadedStrings #-}

module FilterMain where

import           Config
import           Control.Monad.Reader
import           Data.List              (nubBy)
import           Data.Text              (unpack)
import           Database.SQLite.Simple (close, open)
import           Scowl
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  args <- getArgs
  conn <- open connStr
  let query = head args
  let scowlSize = fromInt $ read (args !! 1)
  let config = Config query conn
  wordLists <- loadWordsFromScowl scowlSize
  dbResults <- runReaderT selectResults config
  close conn
  print $ show (length dbResults) ++ " total results"
  let results = nubBy cmpKey $ fmap (\(ResultsField _ _ eq v) -> (,) (unpack eq) (unpack v)) dbResults
        where cmpKey (_, y) (_, y') = y == y'
  let filteredResults = fmap (filterResults query results) wordLists
  output <- traverse (uncurry (writeWordsToFile query)) (zip (enumFromTo S10 scowlSize) filteredResults)
  print $ show (length output) ++ " filtered results"
