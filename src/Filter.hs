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
import           Control.Monad.Reader
import           Data.List            (groupBy, sortBy, (\\))
import qualified Data.Set             as S
import           Model
import           Prelude              hiding (filter)

type Filter = ReaderT FilterConfig IO

data Result = Result
  { result_query  :: String
  , result_value :: String
  }

data FilteredResult = FilteredResult Result Size

instance Eq Result where
  (==) a b = result_value a == result_value b

instance Ord Result where
  (<=) a b = result_value a <= result_value b

instance Eq FilteredResult where
  (==) (FilteredResult r1 _) (FilteredResult r2 _) =
    r1 == r2

instance Ord FilteredResult where
  (<=) (FilteredResult r1 _) (FilteredResult r2 _) =
    result r1 <= result r2

filter :: Query -> [Result] -> Filter [FilteredResult]
filter q results = do
  sws <- asks filter_scowlWordSets
  wordLists <- liftIO $ loadWordLists sws
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
  let resultDiff = S.fromList $ extractExpansion s (result_value res)
  guard $ null (resultDiff S.\\ _words wordList)
  pure $ FilteredResult res (_size wordList)

-- SORTERS
sort :: [FilteredResult] -> [[FilteredResult]]
sort results =
  let sortedResults = sortBy (\(FilteredResult _ size1) (FilteredResult _ size2) -> compare size1 size2) results
  in groupBy (\(FilteredResult _ size1) (FilteredResult _ size2) -> size1 == size2) sortedResults

-- SCOWL WORD LISTS
data WordList = WordList
 { wordList_size  :: Size
 , wordList_words :: S.Set String
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
  WordList (wordList_size $ last wordLists) (foldl (\a b -> a `S.union` wordList_words b) S.empty wordLists)
