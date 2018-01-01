{-# LANGUAGE OverloadedStrings #-}

module Filter
  ( filterResults
  , findExceptionalResults
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
  { _resultLength :: Int
  , _scowlSize :: Size
  , _results :: [String] 
  }

instance Show FilteredResultSet where 
  show frs = show $ _resultLength frs

filterResults :: Query -> Int -> [String] -> FilterConfig -> IO [FilteredResultSet]
filterResults q maxLength results (FilterConfig sws) = do
  wordLists <- loadWordLists sws
  let accWordLists = accumulatedWordLists wordLists
  pure $ filterResultsByLength q maxLength (fromList results, []) accWordLists

filterResultsByLength :: Query -> Int -> (Set String, [FilteredResultSet]) -> [WordList] -> [FilteredResultSet]
filterResultsByLength q maxLength rs@(unsorted, sorted) wordLists = 
  if maxLength == 0 
    then sorted
  else 
    let newSorted = filterResultsByScowlSet q maxLength rs wordLists
    in filterResultsByLength q (maxLength - 1) (unsorted \\ (fromList $ concatMap _results newSorted), newSorted) wordLists

filterResultsByScowlSet :: Query -> Int -> (Set String, [FilteredResultSet]) -> [WordList] -> [FilteredResultSet]
filterResultsByScowlSet q resultLength rs@(unsorted, sorted) scowlSets =
  if null scowlSets
    then sorted
  else 
    let (x:xs) = scowlSets
        newSorted = runFilter q resultLength rs x
        allSorted = sorted ++ [newSorted]
    in filterResultsByScowlSet q resultLength (unsorted \\ (fromList $ _results newSorted), allSorted) xs 

runFilter :: Query -> Int -> (Set String, [FilteredResultSet]) -> WordList -> FilteredResultSet
runFilter (Query _ _ s) resultLength (unsorted, sorted) wordList = 
  let sortedResultList = concatMap _results sorted
      filteredResults = do
        result <- elems unsorted
        let resultDiff = extractExpansion s result
        guard $
          (length resultDiff == resultLength) &&
          null (fromList resultDiff \\ _words wordList) &&
          (result `notElem` sortedResultList)
        pure result
  in FilteredResultSet resultLength (_size wordList) filteredResults

-- FINDERS
findExceptionalResults :: Query -> Bool -> [(String, String)] -> [FilteredResultSet] -> [String]
findExceptionalResults bq@(Query _ _ s) matching allResults frs =
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

-- DATASOURCE
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