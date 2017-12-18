{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( loadWordsFromScowl
  , writeWordsToFile
  , filterResults
  , fromInt
  , Size(..)
  ) where

import           Control.Monad
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

toInt :: Size -> Int
toInt size = read $ drop 1 (show size)

fromInt :: Int -> Size
fromInt int = read $ "S" ++ show int

filterResults :: String -> [(String, String)] -> Set String -> [String]
filterResults bq results scowlList =
  let resultValues = fmap snd results
      bqWords = words bq
  in do
    currentResult <- results
    let rWords = words (fst currentResult)
    let exWords = words (snd currentResult)
    guard $
      (length (exWords !! 1) <= 2) ||
      ((length rWords <= length bqWords + 1) &&
      (bq == head rWords) &&
      null (fromList (tail rWords) \\ scowlList) &&
      (init (fst currentResult) `notElem` resultValues))
    pure $ fst currentResult

loadWordsFromScowl :: Size -> IO [Set String]
loadWordsFromScowl size = traverse loadWordsFromFile (enumFromTo S10 size)

loadWordsFromFile :: Size -> IO (Set String)
loadWordsFromFile size = do
  let fileName = "./scowl/final/english-words." ++ show (toInt size)
  fileContents <- readFile fileName
  pure $ fromList $ lines fileContents

writeWordsToFile :: String -> Size -> [String] -> IO [()]
writeWordsToFile baseQuery size ws =
  sequence $ do
    let fileName = "./output/" ++ baseQuery ++ "_scowlSize=" ++ show size ++ "_resultCount=" ++ show (length ws) ++ ".txt"
    word <- sort ws
    pure $ appendFile fileName (word ++ "\n")
