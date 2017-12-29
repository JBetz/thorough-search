{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( filterResults
  , findExceptionalResults
  , fromInt
  , Size(..)
  , FilteredResultSet(..)
  ) where

import Control.Monad
import Data.List ((\\))
import Model

-- FILTERS
data FilteredResultSet = FilteredResultSet
  { _resultLength :: Int
  , _scowlSize :: Size
  , _results :: [String] 
  }

filterResults :: Query -> [String] -> Int -> IO [FilteredResultSet]
filterResults bq results maxLength = do
  wordLists <- loadWordLists
  let accWordLists = accumulatedWordLists wordLists
  pure $ filterResultsByLength bq maxLength (results, []) accWordLists

filterResultsByLength :: Query -> Int -> ([String], [FilteredResultSet]) -> [WordList] -> [FilteredResultSet]
filterResultsByLength bq maxLength rs@(unsorted, sorted) wordLists = 
  if maxLength == 0 
    then sorted
  else 
    let curSorted = filterResultsByScowlSet bq maxLength rs wordLists
        newSorted = sorted ++ curSorted
    in filterResultsByLength bq (maxLength - 1) (unsorted \\ concatMap _results curSorted, newSorted) wordLists

filterResultsByScowlSet :: Query -> Int -> ([String], [FilteredResultSet]) -> [WordList] -> [FilteredResultSet]
filterResultsByScowlSet bq resultLength rs@(unsorted, sorted) scowlSets =
  if null scowlSets
    then sorted
  else 
    let (x:xs) = scowlSets
        curSorted = runFilter bq resultLength rs x
        newSorted = sorted ++ [curSorted]
    in filterResultsByScowlSet bq resultLength (unsorted \\ _results curSorted, newSorted) xs 

runFilter :: Query -> Int -> ([String], [FilteredResultSet]) -> WordList -> FilteredResultSet
runFilter bq@(Query _ _ s) resultLength (unsorted, sorted) wordList = 
  let filteredResults = do
        result <- unsorted
        let resultDiff = extractExpansion s result
        guard $
          (length resultDiff == resultLength) &&
          (bq `matches` result) &&
          null (resultDiff \\ _words wordList) &&
          (result `notElem` concatMap _results sorted)
        pure result
  in FilteredResultSet resultLength (_size wordList) filteredResults

-- FINDERS
findExceptionalResults :: Query -> [(String, String)] -> [FilteredResultSet] -> [String]
findExceptionalResults bq allResults frs =
  snd <$>
  filter
    (\(query, result) ->
       let rWords = words result
       in (bq `matches` result) &&
          length rWords == 2 &&
          length query <= 2 && 
          result `notElem` (concatMap _results frs)
    )
    allResults

-- DATASOURCE
data WordList = WordList
 { _size :: Size 
 , _words :: [String]
 } 

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

loadWordLists :: IO [WordList]
loadWordLists = traverse loadWordList (enumFromTo S10 S95)

loadWordList :: Size -> IO WordList
loadWordList size = do
  let fileNames =
        fmap
          (\name -> "./scowl/final/" ++ name ++ "." ++ show (toInt size))
          wordSetNames
  fileContents <- traverse readFile fileNames
  pure $ WordList size (join $ fmap lines fileContents)

accumulatedWordLists :: [WordList] -> [WordList]
accumulatedWordLists wordLists =
  fmap (\i -> combine $ take i wordLists) [1 .. length wordLists]

combine :: [WordList] -> WordList
combine wordLists = 
  WordList (_size $ last wordLists) (concatMap (\(WordList _ ws) -> ws) wordLists) 