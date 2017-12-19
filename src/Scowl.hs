{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( loadWordsFromScowl
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

filterResults :: String -> Map String String -> Set String -> [String]
filterResults bq results scowlList =
  let resultValues = elems results
      bqWords = words bq
  in do
    currentResult <- assocs results
    let rWords = words (snd currentResult)
    guard $
      (length rWords <= length bqWords + 1) &&
      (bq == head rWords) &&
      null (fromList (tail rWords) \\ scowlList) &&
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

loadWordsFromScowl :: Size -> IO [Set String]
loadWordsFromScowl size = traverse loadWordsFromFile (enumFromTo S10 size)

loadWordsFromFile :: Size -> IO (Set String)
loadWordsFromFile size = do
  let fileName = "./scowl/final/english-words." ++ show (toInt size)
  fileContents <- readFile fileName
  pure $ fromList $ lines fileContents

writeFilteredWordsToFile :: String -> Size -> [String] -> IO [()]
writeFilteredWordsToFile baseQuery size ws =
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
