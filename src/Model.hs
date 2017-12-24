module Model
  ( Query(..)
  , Structure(..)
  , matches
  , difference
  , readStr
  , show_
  ) where

import Data.List

-- DATATYPES
data Query = Query
  { base :: String 
  , expansion :: String
  , structure :: Structure
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
show_ q@(Query _ _ s) = 
  let sanitized = fmap (\c -> if c == ' ' then '_' else c) (show q) 
    in case s of 
      XWord -> 'X' : sanitized
      _ ->  sanitized ++ "X"

-- PARSING
readStr :: String -> Query
readStr string = 
  let mIndex = elemIndex 'X' string
      strLength = length string
  in case mIndex of
      Just 0 -> Query (drop 2 string) "" XWord 
      Just index -> 
        if index == strLength - 1 
          then Query (take (strLength - 2) string) "" WordX
          else Query (take (strLength - 5) string) "" WordOfX
      Nothing -> error "invalid query, needs to be of form '<word> X', '<word> of X', or 'X <word>'"

-- TESTING
matches :: Query -> String -> Bool
matches (Query b _ s) result = 
  case s of 
    WordX -> (head . words) result == b
    WordOfX -> take 2 (words result) == words b
    XWord -> last (words result) == b

difference :: Query -> String -> [String]
difference (Query _ _ s) result = 
  let rWords = words result
  in case s of 
      WordX -> drop 1 rWords
      WordOfX -> drop 2 rWords
      XWord -> take (length rWords - 2) rWords
