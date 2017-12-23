{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( loadWordSets
  , filterResults
  , writeFilteredWordsToFile
  , writeExceptionalWordsToFile
  , findExceptionalResults
  , fromInt
  , Size(..)
  ) where

import           Control.Monad
import           Data.Char     (isAscii)
import           Data.List     (sort)
import           Data.Set      (Set, fromList, null, (\\))
import           Prelude       hiding (null)

data Size
  = S10
  | S20
  | S35
  | S40
  | S50
  | S55
  | S60
  | S70
  | S80
  | S95
  deriving (Read, Enum, Show)

wordSetNames :: [String]
wordSetNames = ["english-words", "american-words", "british-words"]

toInt :: Size -> Int
toInt size = read $ drop 1 (show size)

fromInt :: Int -> Size
fromInt int = read $ "S" ++ show int

filterResults :: String -> [(String, String)]-> Size -> IO [[String]]
filterResults bq results size = do
  scowlSets <- loadWordSets size
  pure $ fmap (filterResultsWith bq results) scowlSets

filterResultsWith :: String -> [(String, String)] -> Set String -> [String]
filterResultsWith bq results scowlSet =
  let bqWords = words bq
      resultValues = fmap snd results
  in do
    currentResult <- results
    let rWords = words $ snd currentResult
    guard $
      (length rWords <= length bqWords + 2) &&
      (bq == head rWords) &&
      null (fromList (tail rWords) \\ scowlSet) &&
      (init (snd currentResult) `notElem` resultValues)
    pure $ snd currentResult

findExceptionalResults :: String -> [(String, String)] -> [String] -> [String]
findExceptionalResults bq allResults filteredResults =
  snd <$> filter (\(query, value) ->
    let rWords = words value
    in (bq == head rWords) &&
       length rWords == 2 &&
       length (words query !! 1) <= 2 &&
       value `notElem` filteredResults
  ) allResults

loadWordSets :: Size -> IO [Set String]
loadWordSets size = traverse loadWordSet (enumFromTo S10 size)

loadWordSet :: Size -> IO (Set String)
loadWordSet size = do
  let fileNames = fmap (\name -> "./scowl/final/" ++ name ++ "." ++ show (toInt size)) wordSetNames
  fileContents <- traverse readFile fileNames
  pure $ fromList (join $ fmap lines fileContents)

writeFilteredWordsToFile :: String -> [[String]] -> IO [[()]]
writeFilteredWordsToFile baseQuery ws =
  let wordPairs = zip (enumFrom S10) ws
  in traverse (uncurry (writeFilteredWordSetToFile baseQuery)) wordPairs

writeFilteredWordSetToFile :: String -> Size -> [String] -> IO [()]
writeFilteredWordSetToFile baseQuery size ws =
  let filePath = outputFilePath baseQuery "scowl" [("dictionarySize", show size), ("count", show (length ws))]
  in writeWordsToFile filePath ws

writeExceptionalWordsToFile :: String -> [String] -> IO [()]
writeExceptionalWordsToFile baseQuery ws =
  let filePath = outputFilePath baseQuery "exceptional" [("count", show (length ws))]
  in writeWordsToFile filePath ws

writeWordsToFile :: String -> [String] -> IO [()]
writeWordsToFile filePath ws =
  sequence $ do
    word <- sort ws
    pure $ appendFile filePath (filter isAscii word ++ "\n")

outputFilePath :: String -> String -> [(String, String)] -> String 
outputFilePath baseQuery kind metaData = 
  "./output/" ++ baseQuery ++ "/" ++ baseQuery ++ "-" ++ kind ++ (concatMap (\(k, v) -> "_" ++ k ++ "=" ++ v) metaData) ++ ".txt"
