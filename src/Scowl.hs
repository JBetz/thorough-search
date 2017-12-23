{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( loadWordSets
  , filterResults
  , findExceptionalResults
  , fromInt
  , Size(..)
  ) where

import Control.Monad
import Data.Set (Set, (\\), fromList, null)
import Model
import Prelude hiding (null)

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

filterResults :: Query -> [(String, String)] -> Size -> IO [[String]]
filterResults bq results size = do
  scowlSets <- loadWordSets size
  pure $ fmap (filterResultsWith bq results) scowlSets

filterResultsWith :: Query -> [(String, String)] -> Set String -> [String]
filterResultsWith bq results scowlSet =
  let bqWords = words (show bq)
      resultValues = fmap snd results
  in do currentResult <- results
        let rWords = words $ snd currentResult
        guard $
          (length rWords <= length bqWords + 2) &&
          (bq `matches` snd currentResult) &&
          null (fromList (tail rWords) \\ scowlSet) &&
          (init (snd currentResult) `notElem` resultValues)
        pure $ snd currentResult

findExceptionalResults :: Query -> [(String, String)] -> [String] -> [String]
findExceptionalResults bq allResults filteredResults =
  snd <$>
  filter
    (\(query, result) ->
       let rWords = words result
       in (bq `matches` result) &&
          length rWords == 2 &&
          length (words query !! 1) <= 2 && result `notElem` filteredResults)
    allResults

loadWordSets :: Size -> IO [Set String]
loadWordSets size = traverse loadWordSet (enumFromTo S10 size)

loadWordSet :: Size -> IO (Set String)
loadWordSet size = do
  let fileNames =
        fmap
          (\name -> "./scowl/final/" ++ name ++ "." ++ show (toInt size))
          wordSetNames
  fileContents <- traverse readFile fileNames
  pure $ fromList (join $ fmap lines fileContents)
