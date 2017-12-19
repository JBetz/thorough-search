{-# LANGUAGE OverloadedStrings #-}

module FilterMain where

import           Config
import           Control.Monad.Reader
import           Data.Map               (fromList, assocs)
import           Data.Text              (unpack)
import           Data.Tuple             (swap)
import           Database.SQLite.Simple (close, open)
import           Scowl
import           Storage
import           System.Environment     (getArgs)

main :: IO ()
main = do
  -- parse command line arguments
  args <- getArgs
  let query = head args
  let scowlSize = fromInt $ read (args !! 1)
  -- initialize configuration variables
  conn <- open connStr
  let config = Config query conn
  -- get results from database
  dbResults <- runReaderT selectResults config
  close conn
  print $ show (length dbResults) ++ " total results"
  let results = fromList (swap <$> assocs (fromList (map swap $ fmap (\(ResultsField _ _ eq v) -> (,) (unpack eq) (unpack v)) dbResults)))
  print $ show (length results) ++ " unique results"
  -- run scowl filter on results
  wordLists <- loadWordsFromScowl scowlSize
  let filteredResults = fmap (filterResults query results) wordLists
  fResults <- traverse (uncurry (writeFilteredWordsToFile query)) (zip (enumFromTo S10 scowlSize) filteredResults)
  print $ show ((length . join) fResults) ++ " scowl filtered results"
  -- find highly relevant words that were excluded by scowl
  let exceptionalResults = findExceptionalResults query results (join filteredResults)
  eResults <- writeExceptionalWordsToFile query exceptionalResults
  print $ show (length eResults) ++ " exceptional results"
