{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Storage
  ( ResultsRow(..)
  , createQueriesTable
  , createResultsTable
  , insertResultList
  , allResults
  , selectQueryResultCount
  , ranQuery
  , commit
  , commitRaw
  ) where

import           Data.Char              (isAscii)
import           Data.Foldable          (traverse_)
import           Data.Function          (on)
import           Data.List              (sort)
import           Data.Map               (assocs, fromList)
import           Data.String            (fromString)
import           Data.Text              (Text, unpack)
import           Data.Tuple             (swap)
import           Database.SQLite.Simple as SQL hiding (Query)
import           Filter                 hiding (filter, sort)
import           Model                  hiding (fromString)
import           System.Directory       (createDirectoryIfMissing)

-- DATABASE
data QueriesRow =
  QueriesRow Int Text Text Text Int
  deriving (Show)

data ResultsRow =
  ResultsRow Int Text Text
  deriving (Show)

instance FromRow ResultsRow where
  fromRow = ResultsRow <$> field <*> field <*> field

instance ToRow ResultsRow where
  toRow (ResultsRow id_ q r) = toRow (id_, q, r)

instance FromRow QueriesRow where
  fromRow = QueriesRow <$> field <*> field <*> field <*> field <*> field

instance ToRow QueriesRow where
  toRow (QueriesRow id_ s b e rc) = toRow (id_, s, b, e, rc)

createQueriesTable :: Query -> Connection -> IO ()
createQueriesTable q conn = do
  execute_
    conn
    (fromString $
      "CREATE TABLE IF NOT EXISTS " ++
      showFormattedQuery q ++
      "_queries (id INTEGER PRIMARY KEY, structure TEXT, base TEXT, expansion TEXT UNIQUE, result_count INTEGER)")

createResultsTable :: Query -> Connection -> IO ()
createResultsTable q conn = do
  execute_
    conn
    (fromString $
      "CREATE TABLE IF NOT EXISTS " ++
      showFormattedQuery q ++
      "_results (id INTEGER PRIMARY KEY, query TEXT, result TEXT UNIQUE)")

insertResultList :: (Query, [String]) -> Connection -> IO ()
insertResultList (q, results) conn = do
  insertQuery q (length results) conn
  traverse_ (\r -> insertResult q r conn) results

insertQuery :: Query -> Int -> Connection -> IO ()
insertQuery q@(Query b e s) resultCount conn =
  execute
    conn
    (fromString $
      "INSERT OR IGNORE INTO " ++
      showFormattedQuery q ++ "_queries (structure, base, expansion, result_count) VALUES (?, ?, ?, ?)")
    (toRow (show s, b, e, resultCount))

insertResult :: Query -> String -> Connection -> IO ()
insertResult q res conn =
  execute
    conn
    (fromString $
      "INSERT OR IGNORE INTO " ++
      showFormattedQuery q ++ "_results (query, result) VALUES (?, ?)")
    [show q, res]

allResults :: Query -> Connection -> IO [Result]
allResults baseQuery conn = do
  totalResults <- query_ conn (fromString $ "SELECT * FROM " ++ showFormattedQuery baseQuery ++ "_results")
  let resultPairs = fmap (\(ResultsRow _ eq r) -> (unpack eq, unpack r)) totalResults
      resultMap = fromList $ fmap swap resultPairs
      uniqueResults = filter (\r -> matches baseQuery (snd r)) (fmap swap (assocs resultMap))
  pure $ fmap (\(q, r) -> Result q r) uniqueResults

ranQuery :: Query -> Connection -> IO Bool
ranQuery q@(Query _ e _) conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT * FROM " ++ showFormattedQuery q ++ "_queries WHERE expansion = ?")
      [e] :: IO [QueriesRow]
  pure $ not (null res)

selectQueryResultCount :: Query -> Connection -> IO Int
selectQueryResultCount q@(Query _ e _) conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT result_count FROM " ++ showFormattedQuery q ++ "_queries WHERE expansion = ?")
      [e] :: IO [Only Int]
  pure $ (fromOnly . head) res

-- FILE
commitRaw :: Query -> [Result] -> IO ()
commitRaw query results = do
  let outputFile = "./output/" ++ showFormattedQuery query ++ ".txt"
  traverse_ (\result -> appendFile outputFile $ result_value result ++ "\n") results

commit :: Query -> [[FilteredResult]] -> IO [()]
commit q filteredResults = do
  let counts = fmap length filteredResults
  let filePath = outputFilePath q (sum counts)
  _ <- createDirectoryIfMissing False ("./output/" ++ showFormattedQuery q)
  traverse (writeWordsToFile filePath) (zip (cumulativePercentages counts) filteredResults)

writeWordsToFile :: String -> (Int, [FilteredResult]) -> IO ()
writeWordsToFile filePath (cumulativePercentage, filteredResults) = do
  _ <- sequence $ do
    (FilteredResult result _) <- sort filteredResults
    pure $ appendFile filePath (filter isAscii (result_value result) ++ "\n")
  let separators = take 20 (repeat '=')
  appendFile filePath $ "\n" ++ separators ++ " " ++ show cumulativePercentage ++ "% " ++ separators ++ "\n\n"

outputFilePath :: Query -> Int -> String
outputFilePath query count =
  "./output/" ++ showFormattedQuery query ++ "-" ++ sizeMessage count ++ ".txt"

cumulativePercentages :: [Int] -> [Int]
cumulativePercentages counts =
  let runningCounts = scanl1 (+) counts
      total = last runningCounts
      floatDiv = (/) `on` fromIntegral :: Int -> Int -> Float
  in fmap (\rc -> round $ (rc * 100) `floatDiv` total) runningCounts

sizeMessage :: Int -> String
sizeMessage count
  | count < 10000 = "under10K"
  | count < 20000 = "under20K"
  | count < 30000 = "under30K"
  | otherwise     = "over30K-WARNING"
