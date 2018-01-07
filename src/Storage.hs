{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Storage
  ( ResultsRow(..)
  , createQueriesTable
  , createResultsTable
  , insertResultList
  , selectAllResults
  , selectUniqueResults
  , selectQueryResultCount
  , ranQuery
  , writeFilteredWordsToFile
  , emailResults
  ) where

import Data.Char (isAscii)
import Data.Function (on)
import Data.List (sort)
import Data.Map (assocs, fromList)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Database.SQLite.Simple as SQL hiding (Query)
import Filter
import Model hiding (fromString)
import Network.Mail.SMTP
import System.Directory (createDirectoryIfMissing)

-- DATABASE
data QueriesRow =
  QueriesRow Int Text Text Text Int
  deriving (Show)

data ResultsRow =
  ResultsRow Int Text Text
  deriving (Show)

--instance FromRow Int where
--  fromRow = field

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
      show_ q ++
      "_queries (id INTEGER PRIMARY KEY, structure TEXT, base TEXT, expansion TEXT UNIQUE, result_count INTEGER)")

createResultsTable :: Query -> Connection -> IO ()
createResultsTable q conn = do
  execute_
    conn
    (fromString $
      "CREATE TABLE IF NOT EXISTS " ++
      show_ q ++
      "_results (id INTEGER PRIMARY KEY, query TEXT, result TEXT UNIQUE)")

insertResultList :: (Query, [String]) -> Connection -> IO Int
insertResultList (q, results) conn = do
  _ <- insertQuery q (length results) conn
  rs <- traverse (\r -> insertResult q r conn) results 
  pure $ length rs

insertQuery :: Query -> Int -> Connection -> IO ()
insertQuery q@(Query b e s) resultCount conn =
  execute
    conn
    (fromString $
      "INSERT OR IGNORE INTO " ++
      show_ q ++ "_queries (structure, base, expansion, result_count) VALUES (?, ?, ?, ?)")
    (toRow (show s, b, e, resultCount))

insertResult :: Query -> String -> Connection -> IO ()
insertResult q result conn =
  execute
    conn
    (fromString $
      "INSERT OR IGNORE INTO " ++
      show_ q ++ "_results (query, result) VALUES (?, ?)")
    [show q, result]

selectUniqueResults :: Query -> Connection -> IO [(String, String)]
selectUniqueResults q conn = do
  totalResults <- selectAllResults q conn
  let resultPairs = fmap (\(ResultsRow _ eq v) -> (unpack eq, unpack v)) totalResults
  let resultMap = fromList $ fmap swap resultPairs
  pure $ fmap swap (assocs resultMap)

selectAllResults :: Query -> Connection -> IO [ResultsRow]
selectAllResults q conn =
  query_ conn (fromString $ "SELECT * FROM " ++ show_ q ++ "_results")

ranQuery :: Query -> Connection -> IO Bool
ranQuery q@(Query _ e _) conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT * FROM " ++ show_ q ++ "_queries WHERE expansion = ?")
      [e] :: IO [QueriesRow]
  pure $ not (null res)

selectQueryResultCount :: Query -> Connection -> IO Int
selectQueryResultCount q@(Query _ e _) conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT result_count FROM " ++ show_ q ++ "_queries WHERE expansion = ?")
      [e] :: IO [Only Int]
  pure $ (fromOnly . head) res

-- FILE 
writeFilteredWordsToFile :: Query -> [FilteredResultSet] -> IO [()]
writeFilteredWordsToFile q frs = do
  _ <- createDirectoryIfMissing False ("./output/" ++ show_ q)
  let counts = fmap (length . _results) frs
  traverse (writeFilteredResultSetToFile q) (zip (cumulativePercentages counts) frs)

writeFilteredResultSetToFile :: Query -> (Int, FilteredResultSet) -> IO ()
writeFilteredResultSetToFile q (cp, (FilteredResultSet _ ws)) =
  let filePath = outputFilePath q (length ws)
  in writeWordsToFile filePath ws cp

writeWordsToFile :: String -> [String] -> Int -> IO ()
writeWordsToFile filePath ws cp = do
  sequence $ do
    word <- sort ws
    pure $ appendFile filePath (filter isAscii word ++ "\n")
  let separators = take 20 (repeat '=')
  appendFile filePath $ "\n" ++ separators ++ " " ++ show cp ++ "% " ++ separators ++ "\n"

outputFilePath :: Query -> Int -> String
outputFilePath q count =
  "./output/" ++ show_ q ++ "-" ++ sizeMessage count ++ ".txt"

cumulativePercentages :: [Int] -> [Int]
cumulativePercentages counts =
  let runningCounts = scanl1 (+) counts
      total = last runningCounts
      floatDiv = (/) `on` fromIntegral :: Int -> Int -> Float
  in fmap (\rc -> round $ (rc * 100) `floatDiv` total) runningCounts

sizeMessage :: Int -> String
sizeMessage count 
  | count <= 5000  = "<<10K"
  | count <= 10000 = "<10K"
  | count <= 20000 = "<20K"
  | count <= 30000 = "<30K"
  | otherwise      = ">30K-WARNING"

-- EMAIL
emailResults :: FilePath -> IO ()
emailResults fp = 
  let from       = Address Nothing ""
      to         = [Address (Just "Jason Hickner") ""]
      cc         = []
      bcc        = []
      subject    = "email subject"
      body       = plainTextPart "email body"
      html       = htmlPart "<h1>HTML</h1>"
      host       = ""
  in do
    attachment <- filePart "application/octet-stream" fp
    let mail = simpleMail from to cc bcc subject [body, html, attachment]
    sendMail host mail
