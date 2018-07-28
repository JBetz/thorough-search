{-# LANGUAGE OverloadedStrings #-}

module Filter
  ( filter
  , sort
  , fromInt
  , Size(..)
  , Result(..)
  , FilteredResult(..)
  ) where

import           Config
import           Control.Monad
import           Data.List     (groupBy, sortBy, (\\))
import qualified Data.Set      as S
import           Model
import           Prelude       hiding (filter)

-- FILTERS
data Result = Result
  { query  :: String
  , result :: String
  }

data FilteredResult = FilteredResult Result Size

queryLength :: FilteredResult -> Int
queryLength (FilteredResult r _) = length (query r)

instance Eq Result where
  (==) a b = result a == result b

instance Ord Result where
  (<=) a b = result a <= result b

instance Eq FilteredResult where
  (==) (FilteredResult r1 _) (FilteredResult r2 _) =
    r1 == r2

instance Ord FilteredResult where
  (<=) (FilteredResult r1 _) (FilteredResult r2 _) =
    result r1 <= result r2

filter :: Query -> [Result] -> FilterConfig -> IO [FilteredResult]
filter q results (FilterConfig sws) = do
  wordLists <- loadWordLists sws
  let accWordLists = accumulatedWordLists wordLists
  pure $ byScowlSet q results [] accWordLists

byScowlSet :: Query -> [Result] -> [FilteredResult] -> [WordList] -> [FilteredResult]
byScowlSet _ _ filtered [] = filtered
byScowlSet q unfiltered filtered wordLists =
  let (x:xs) = wordLists
      newFiltered = runFilter q unfiltered x
      allFiltered = filtered ++ newFiltered
  in byScowlSet q (unfiltered \\ ((\(FilteredResult r _) -> r) <$> newFiltered)) allFiltered xs

runFilter :: Query -> [Result] -> WordList -> [FilteredResult]
runFilter (Query _ _ s) unfiltered wordList = do
  res <- unfiltered
  let resultDiff = S.fromList $ extractExpansion s (result res)
  guard $ null (resultDiff S.\\ _words wordList)
  pure $ FilteredResult res (_size wordList)

-- SORTERS
sort :: [FilteredResult] -> [[FilteredResult]]
sort results =
  let sortedResults = sortBy (\(FilteredResult _ size1) (FilteredResult _ size2) -> compare size1 size2) results
  in groupBy (\(FilteredResult _ size1) (FilteredResult _ size2) -> size1 == size2) sortedResults

sortByQueryLength :: [FilteredResult] -> [[FilteredResult]]
sortByQueryLength results =
  let sortedResults = sortBy (\r1 r2 -> compare (queryLength r1) (queryLength r2)) results
  in groupBy (\r1 r2 -> queryLength r1 == queryLength r2) sortedResults

-- SCOWL WORD LISTS
data WordList = WordList
 { _size  :: Size
 , _words :: S.Set String
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
  deriving (Eq, Ord, Read, Enum, Show)

toInt :: Size -> Int
toInt size = read $ drop 1 (show size)

fromInt :: Int -> Size
fromInt int = read $ "S" ++ show int

loadWordLists :: [String] -> IO [WordList]
loadWordLists names = traverse (`loadWordList` names) (enumFromTo S10 S95)

loadWordList :: Size -> [String] -> IO WordList
loadWordList size names = do
  let fileNames = fmap (\n -> "./scowl/" ++ n ++ "." ++ show (toInt size)) names
  fileContents <- traverse readFile fileNames
  pure $ WordList size (S.fromList $ join (fmap lines fileContents))

accumulatedWordLists :: [WordList] -> [WordList]
accumulatedWordLists wordLists =
  fmap (\i -> combine $ take i wordLists) [1 .. length wordLists]

combine :: [WordList] -> WordList
combine wordLists =
  WordList (_size $ last wordLists) (foldl (\a b -> a `S.union` _words b) S.empty wordLists)
