{-# LANGUAGE OverloadedStrings #-}

module Filter
  ( filterResults
  , filterExceptionalResults
  , fromInt
  , Size(..)
  , FilteredResultSet(..)
  ) where

import Config
import Control.Monad
import Data.Set (Set, (\\), union, fromList, empty, elems)
import Model

-- FILTERS
data FilteredResultSet = FilteredResultSet
  { _scowlSize :: Size
  , _results :: [String] 
  }

filterResults :: Query -> [String] -> FilterConfig -> IO [FilteredResultSet]
filterResults q results (FilterConfig sws) = do
  wordLists <- loadWordLists sws
  let accWordLists = accumulatedWordLists wordLists
  pure $ filterResultsByScowlSet q (fromList results, []) accWordLists

filterResultsByScowlSet :: Query -> (Set String, [FilteredResultSet]) -> [WordList] -> [FilteredResultSet]
filterResultsByScowlSet q rs@(unsorted, sorted) scowlSets =
  if null scowlSets
    then sorted
  else 
    let (x:xs) = scowlSets
        newSorted = runFilter q rs x
        allSorted = sorted ++ [newSorted]
    in filterResultsByScowlSet q (unsorted \\ (fromList $ _results newSorted), allSorted) xs 

runFilter :: Query -> (Set String, [FilteredResultSet]) -> WordList -> FilteredResultSet
runFilter (Query _ _ s) (unsorted, sorted) wordList = 
  let sortedResultList = concatMap _results sorted
      filteredResults = do
        result <- elems unsorted
        let resultDiff = extractExpansion s result
        guard $
          null (fromList resultDiff \\ _words wordList) &&
          (result `notElem` sortedResultList)
        pure result
  in FilteredResultSet (_size wordList) filteredResults

filterExceptionalResults :: Query -> Bool -> [(String, String)] -> [FilteredResultSet] -> [String]
filterExceptionalResults bq@(Query _ _ s) matching allResults frs =
  let filteredResults = concatMap _results frs
  in snd <$>
      filter
        (\(query, result) ->
          let match = bq `matches` result
          in (if matching then match else not match) &&
             length (concat $ extractExpansion s query) <= 2 && 
             result `notElem` filteredResults
        )
        allResults

-- SCOWL WORD LISTS
data WordList = WordList
 { _size :: Size 
 , _words :: Set String
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

toInt :: Size -> Int
toInt size = read $ drop 1 (show size)

fromInt :: Int -> Size
fromInt int = read $ "S" ++ show int

loadWordLists :: [String] -> IO [WordList]
loadWordLists names = traverse (`loadWordList` names) (enumFromTo S10 S95)

loadWordList :: Size -> [String] -> IO WordList
loadWordList size names = do
  let fileNames = fmap (\n -> "./scowl/final/" ++ n ++ "." ++ show (toInt size)) names
  fileContents <- traverse readFile fileNames
  pure $ WordList size (fromList $ join (fmap lines fileContents))

accumulatedWordLists :: [WordList] -> [WordList]
accumulatedWordLists wordLists =
  fmap (\i -> combine $ take i wordLists) [1 .. length wordLists]

combine :: [WordList] -> WordList
combine wordLists = 
  WordList (_size $ last wordLists) (foldl (\a b -> a `union` _words b) empty wordLists) 