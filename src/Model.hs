module Model
  ( Query(..)
  , expansion
  , matches
  , readStr
  , serialize
  , deserialize
  , show_
  ) where

import Data.List

data Query
  = WordX String String
  | WordOfX String String
  | XWord String String

instance Show Query where
  show query =
    case query of
      WordX base ex -> base ++ " " ++ ex
      WordOfX base ex -> base ++ " of " ++ ex
      XWord base ex -> ex ++ " " ++ base

expansion :: Query -> String
expansion query = 
  case query of 
    WordX _ ex -> ex
    WordOfX _ ex -> ex
    XWord _ ex -> ex

matches :: Query -> String -> Bool
matches query result = 
  case query of 
    WordX base _ -> (head . words) result == base
    WordOfX base _ -> take 2 (words result) == words base
    XWord base _ -> last (words result) == base 
 

readStr :: String -> Query
readStr string = 
  let mIndex = elemIndex 'X' string
      strLength = length string
  in case mIndex of
    Just 0 -> XWord (drop 2 string) "" 
    Just index -> 
        if index == strLength - 1 
            then WordX (take (strLength - 2) string) ""
            else WordOfX (take (strLength - 5) string) ""
    Nothing -> error "invalid query, needs to be of form '<word> X', '<word> of X', or 'X <word>'"

serialize :: Query -> (String, String, String)
serialize query =
  case query of
    WordX base ex -> ("WordX", base, ex)
    WordOfX base ex -> ("WordOfX", base, ex)
    XWord base ex -> ("XWord", base, ex)

deserialize :: (String, String, String) -> Query
deserialize (str, base, ex) =
  case str of
    "WordX" -> WordX base ex
    "WordOfX" -> WordOfX base ex
    "XWord" -> XWord base ex

show_ :: Query -> String
show_ q = 
  let sanitized = fmap (\c -> if c == ' ' then '_' else c) (show q) 
  in case q of 
       XWord _ _ -> 'X' : sanitized
       _ ->  sanitized ++ "X"
