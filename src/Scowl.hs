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
import           Data.List     (sort)
import           Data.Map      (Map, elems, assocs)
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

toInt :: Size -> Int
toInt size = read $ drop 1 (show size)

fromInt :: Int -> Size
fromInt int = read $ "S" ++ show int

filterResults :: String -> Map String String -> Size -> IO [[String]]
filterResults bq results size = do
  scowlSets <- loadWordSets size
  pure $ fmap (filterResultsWith bq results) scowlSets

filterResultsWith :: String -> Map String String -> Set String -> [String]
filterResultsWith bq results scowlSet =
  let resultValues = elems results
      bqWords = words bq
  in do
    currentResult <- assocs results
    let rWords = words $ snd currentResult
    guard $
      (length rWords <= length bqWords + 1) &&
      (bq == head rWords) &&
      null (fromList (tail rWords) \\ scowlSet) &&
      (init (fst currentResult) `notElem` resultValues)
    pure $ snd currentResult

findExceptionalResults :: String -> Map String String -> [String] -> [String]
findExceptionalResults bq allResults filteredResults =
  snd <$> filter (\(query, value) ->
    let rWords = words value
    in (bq == head rWords) &&
       length rWords == 2 &&
       length (words query !! 1) <= 2 &&
       value `notElem` filteredResults
  ) (assocs allResults)

loadWordSets :: Size -> IO [Set String]
loadWordSets size = traverse loadWordSet (enumFromTo S10 size)

loadWordSet :: Size -> IO (Set String)
loadWordSet size = do
  let fileName = "./scowl/final/english-words." ++ show (toInt size)
  fileContents <- readFile fileName
  pure $ fromList (lines fileContents)

writeFilteredWordsToFile :: String -> [[String]] -> IO [[()]]
writeFilteredWordsToFile baseQuery ws =
  let wordPairs = zip (enumFrom S10) ws
  in traverse (uncurry (writeFilteredWordSetToFile baseQuery)) wordPairs

writeFilteredWordSetToFile :: String -> Size -> [String] -> IO [()]
writeFilteredWordSetToFile baseQuery size ws =
  let fileName = "./output/" ++ baseQuery ++ "_scowlSize=" ++ show size ++ "_count=" ++ show (length ws) ++ ".txt"
  in writeWordsToFile fileName ws

writeExceptionalWordsToFile :: String -> [String] -> IO [()]
writeExceptionalWordsToFile baseQuery ws =
  let fileName = "./output/" ++ baseQuery ++ "_exceptional_count=" ++ show (length ws) ++ ".txt"
  in writeWordsToFile fileName ws

writeWordsToFile :: String -> [String] -> IO [()]
writeWordsToFile fileName ws =
  sequence $ do
    word <- sort ws
    pure $ appendFile fileName (word ++ "\n")
