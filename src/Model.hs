module Model
  ( Query(..)
  , Structure(..)
  , matches
  , extractExpansion
  , fromString
  , show_
  ) where

import Data.List

-- DATATYPES
data Query = Query
  { _base :: String 
  , _expansion :: String
  , _structure :: Structure
  }

data Structure = WordX | WordOfX | XWord
  deriving (Show)

-- PRINTING
instance Show Query where
  show (Query b e s) =
    case s of
      WordX -> b ++ " " ++ e
      WordOfX -> b ++ " of " ++ e
      XWord -> e ++ " " ++ b

show_ :: Query -> String
show_ (Query b _ s) = 
  case s of
    WordX -> b ++ "_X"
    WordOfX -> b ++ "_of_X"
    XWord -> "X_" ++ b

-- PARSING
fromString :: String -> Query
fromString string = 
  let mIndex = elemIndex 'X' string
      strLength = length string
      errorMessage = "invalid query, needs to be of form '<word> X', '<word> of X', or 'X <word>'"
  in case mIndex of
      Just 0 -> Query (drop 2 string) "" XWord 
      Just index -> 
        if index == strLength - 1
          then if " of " `isInfixOf` string 
            then Query (take (strLength - 5) string) "" WordOfX
            else Query (take (strLength - 2) string) "" WordX
          else error errorMessage
      Nothing -> error errorMessage

-- TESTING
matches :: Query -> String -> Bool
matches (Query b _ s) result = 
  let rWords = words result
  in case s of 
      WordX -> head rWords == b
      WordOfX -> take 2 rWords == [b] ++ ["of"]
      XWord -> last rWords == b

extractExpansion :: Structure -> String -> [String]
extractExpansion s str = 
  let sWords = words str
  in case s of 
      WordX -> drop 1 sWords
      WordOfX -> drop 2 sWords
      XWord -> init sWords
