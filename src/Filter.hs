{-# LANGUAGE OverloadedStrings #-}

module Filter
  ( filter
  , sort
  , fromInt
  , loadWordList
  , loadWordLists
  , Size(..)
  , Result(..)
  , FilteredResult(..)
  ) where

import           Config
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.List            (groupBy, sortBy, (\\))
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Text.Encoding                  (decodeUtf8With)
import           Data.Text.Encoding.Error            (lenientDecode)
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
    result_value r1 <= result_value r2

filter :: Query -> [Result] -> Filter [FilteredResult]
filter query results = do
  sws <- asks filter_scowlWordSets
  wordLists <- loadWordLists sws
  let accWordLists = accumulatedWordLists wordLists
  pure $ byScowlSet query results [] accWordLists

byScowlSet :: Query -> [Result] -> [FilteredResult] -> [WordList] -> [FilteredResult]
byScowlSet _ _ filtered [] = filtered
byScowlSet query unfiltered filtered wordLists =
  let (x:xs) = wordLists
      newFiltered = runFilter query unfiltered x
      allFiltered = filtered ++ newFiltered
  in byScowlSet query (unfiltered \\ ((\(FilteredResult result _) -> result) <$> newFiltered)) allFiltered xs

runFilter :: Query -> [Result] -> WordList -> [FilteredResult]
runFilter (Query _ _ structure) unfiltered wordList = do
  result <- unfiltered
  let resultDiff = S.fromList $ extractExpansion structure (result_value result)
  guard $ null (resultDiff S.\\ wordList_words wordList)
  pure $ FilteredResult result (wordList_size wordList)

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

loadWordLists :: [String] -> Filter [WordList]
loadWordLists names = traverse (`loadWordList` names) (enumFromTo S10 S95)

loadWordList :: Size -> [String] -> Filter WordList
loadWordList size names = do
  scowlFilePath <- asks filter_scowlFilePath
  let fileNames = fmap (\n -> "./" ++ scowlFilePath ++ "/" ++ n ++ "." ++ show (toInt size)) names
  fileContents <- traverse (\fileName -> do
    fileContent <- liftIO $ BS.readFile fileName
    pure $ decodeUtf8With lenientDecode fileContent) fileNames
  pure $ WordList size (S.fromList $ join (lines . T.unpack <$> fileContents))

accumulatedWordLists :: [WordList] -> [WordList]
accumulatedWordLists wordLists =
  fmap (\i -> combine $ take i wordLists) [1 .. length wordLists]

combine :: [WordList] -> WordList
combine wordLists =
  WordList (wordList_size $ last wordLists) (foldl (\a b -> a `S.union` wordList_words b) S.empty wordLists)
