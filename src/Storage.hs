{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Storage
  ( ResultsField(..)
  , createQueriesTable
  , createResultsTable
  , insertResultList
  , selectAllResults
  , selectUniqueResults
  , selectQueryResults
  , ranQuery
  , writeFilteredWordsToFile
  , writeExceptionalWordsToFile
  , archiveResults
  , emailResults
  ) where

import Codec.Archive.Zip
import Control.Monad.Reader
import Data.Char (isAscii)
import Data.List (sort)
import Data.Map (assocs, fromList)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Database.SQLite.Simple as SQL hiding (Query)
import Filter
import Model hiding (fromString)
import Network.Mail.SMTP
import Path (stripProperPrefix)
import Path.IO (resolveDir', resolveFile')
import System.Directory (createDirectoryIfMissing)

-- DATABASE
data QueriesField =
  QueriesField Int
               Text
               Text
               Text
  deriving (Show)

data ResultsField =
  ResultsField Int
               Text
               Text
  deriving (Show)

instance FromRow ResultsField where
  fromRow = ResultsField <$> field <*> field <*> field

instance ToRow ResultsField where
  toRow (ResultsField id_ q r) = toRow (id_, q, r)

instance FromRow QueriesField where
  fromRow = QueriesField <$> field <*> field <*> field <*> field

createQueriesTable :: Query -> Connection -> IO ()
createQueriesTable q conn = do
  execute_
    conn
    (fromString $
      "CREATE TABLE IF NOT EXISTS " ++
      show_ q ++
      "_queries (id INTEGER PRIMARY KEY, structure TEXT, base TEXT, expansion TEXT UNIQUE)")

createResultsTable :: Query -> Connection -> IO ()
createResultsTable q conn = do
  execute_
    conn
    (fromString $
      "CREATE TABLE IF NOT EXISTS " ++
      show_ q ++
      "_results (id INTEGER PRIMARY KEY, query TEXT, result TEXT, CONSTRAINT UC_results UNIQUE (query, result))")

insertResultList :: (Query, [String]) -> Connection -> IO [()]
insertResultList (q, results) conn = do
  _ <- insertQuery q conn
  traverse (\r -> insertResult q r conn) results 

insertQuery :: Query -> Connection -> IO ()
insertQuery q@(Query b e s) conn =
  execute
    conn
    (fromString $
      "INSERT OR IGNORE INTO " ++
      show_ q ++ "_queries (structure, base, expansion) VALUES (?, ?, ?)")
    [show s, b, e]

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
  let resultPairs = fmap (\(ResultsField _ eq v) -> (unpack eq, unpack v)) totalResults
  let resultMap = fromList $ fmap swap resultPairs
  pure $ fmap swap (assocs resultMap)

selectAllResults :: Query -> Connection -> IO [ResultsField]
selectAllResults q conn =
  query_ conn (fromString $ "SELECT * FROM " ++ show_ q ++ "_results")

ranQuery :: Query -> Connection -> IO Bool
ranQuery q@(Query _ e _) conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT * FROM " ++ show_ q ++ "_queries WHERE expansion = ?")
      [e] :: IO [QueriesField]
  pure $ not (null res)

selectQueryResults :: Query -> Connection -> IO (Query, [String])
selectQueryResults q conn = do
  res <-
    SQL.query
      conn
      (fromString $ "SELECT * FROM " ++ show_ q ++ "_results WHERE query = ?")
      [show q] :: IO [ResultsField]
  pure (q, fmap (\(ResultsField _ _ v) -> unpack v) res)

-- FILE 
writeFilteredWordsToFile :: Query -> [FilteredResultSet] -> IO [[()]]
writeFilteredWordsToFile q frs = do
  _ <- createDirectoryIfMissing False ("./output/" ++ show_ q)
  traverse (writeFilteredResultSetToFile q) frs

writeFilteredResultSetToFile :: Query -> FilteredResultSet -> IO [()]
writeFilteredResultSetToFile q (FilteredResultSet rl ss ws) =
  let filePath =
        outputFilePath
          q
          ("length=" ++ show rl)
          [ ("scowlSize", show ss)
          , ("count", show (length ws))
          ]
  in do 
    createDirectoryIfMissing False ("./output/" ++ show_ q ++ "/length=" ++ show rl)
    writeWordsToFile filePath ws

writeExceptionalWordsToFile :: Query -> [String] -> [String] -> IO [()]
writeExceptionalWordsToFile q mws nmws =
  let mFilePath = outputFilePath q "exceptional" [("matchingCount", show (length mws))]
      nmFilePath = outputFilePath q "exceptional" [("nonMatchingCount", show (length nmws))]
  in do 
    createDirectoryIfMissing False ("./output/" ++ show_ q ++ "/exceptional")
    writeWordsToFile mFilePath mws
    writeWordsToFile nmFilePath nmws

writeWordsToFile :: String -> [String] -> IO [()]
writeWordsToFile filePath ws =
  sequence $ do
    word <- sort ws
    pure $ appendFile filePath (filter isAscii word ++ "\n")

outputFilePath :: Query -> String -> [(String, String)] -> String
outputFilePath q subDir metaData =
  "./output/" ++
  show_ q ++
  "/" ++ 
  subDir ++
  "/" ++
  show_ q ++
  "-" ++
  tail (concatMap (\(k, v) -> "_" ++ k ++ "=" ++ v) metaData) ++ ".txt"

-- ARCHIVE
archiveResults :: String -> IO ()
archiveResults src = do
  srcPath <- resolveDir' src
  destPath <- resolveFile' (src ++ ".zip")
  let f = stripProperPrefix srcPath >=> mkEntrySelector
  createArchive destPath (packDirRecur BZip2 f srcPath)

-- EMAIL
emailResults :: FilePath -> IO ()
emailResults fp = 
  let from       = Address Nothing "joebetz91@gmail.com"
      to         = [Address (Just "Jason Hickner") "joebetz91@gmail.com"]
      cc         = []
      bcc        = []
      subject    = "email subject"
      body       = plainTextPart "email body"
      html       = htmlPart "<h1>HTML</h1>"
      host       = "73.176.149.193"
  in do
    attachment <- filePart "application/octet-stream" fp
    let mail = simpleMail from to cc bcc subject [body, html, attachment]
    sendMail host mail
