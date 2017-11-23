{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( completeInstasearch
    , instasearch
    ) where

import Network.Wreq
import Control.Lens
import Control.Concurrent
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.List (isSuffixOf, sort)
import Data.Set (Set, fromList, union, difference, empty, elems, notMember)
import Data.Text (pack)

alphabet :: String
alphabet =
  "abcdefghijklmnopqrstuvwzyz"

completeInstasearch :: String -> Int -> IO [String]
completeInstasearch query maxWordCount = do
  (_, initialResults) <- instasearch query
  results <- runCompleteInstasearch (query ++ " ") (fromList initialResults)
  pure $ sort (filter (\r -> length (words r) <= maxWordCount) results)

runCompleteInstasearch :: String -> Set String -> IO [String]
runCompleteInstasearch query seen = do
  print $ show (length (filter (\r -> length (words r) <= 2) (elems seen))) ++ " - " ++ query
  results <- sequence $ fmap instasearch (generateQueries query seen)
  let newSeen = seen `union` fromList (concatMap snd results)
  recursiveResults <- sequence $ fmap (\r -> runCompleteInstasearch r newSeen) (filterResults results)
  pure $ concatMap snd results ++ concat recursiveResults

instasearch :: String -> IO (String, [String])
instasearch search = do
  threadDelay 500000
  let opts = defaults & param "q" .~ [pack search] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) search

generateQueries :: String -> Set String -> [String]
generateQueries baseQuery seen =
  let isValid q = notMember q seen && not ("  " `isSuffixOf` q)
  in filter isValid (fmap (snoc baseQuery) alphabet)

filterResults :: [(String, [String])] -> [String]
filterResults results =
  fmap fst (filter (\result -> length (snd result) == 10) results)

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left err -> (def, [])
    Right (key, vals) -> (key, vals)
