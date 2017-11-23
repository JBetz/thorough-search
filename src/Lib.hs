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
import Data.List (isSuffixOf, sort, union)
import qualified Data.Set as S
import Data.Text (pack)

alphabet :: String
alphabet =
  "abcdefghijklmnopqrstuvwzyz"

completeInstasearch :: String -> Int -> IO [String]
completeInstasearch query maxWordCount = do
  (_, initialResults) <- instasearch query
  results <- runCompleteInstasearch (query ++ " ")
  pure $ sort (filter (\r -> length (words r) <= maxWordCount) (initialResults `union` results))

runCompleteInstasearch :: String -> IO [String]
runCompleteInstasearch query = do
  results <- sequence $ fmap instasearch (expandQuery query)
  recursiveResults <- sequence $ fmap runCompleteInstasearch (findExpandables results)
  let totalResults = concatMap snd results `union` concat recursiveResults
  print $ show (length totalResults) ++ " from " ++ query
  pure totalResults

instasearch :: String -> IO (String, [String])
instasearch search = do
  threadDelay 500000
  let opts = defaults & param "q" .~ [pack search] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) search

expandQuery :: String -> [String]
expandQuery baseQuery =
  let isValid q = not ("  " `isSuffixOf` q)
  in filter isValid (fmap (snoc baseQuery) alphabet)

findExpandables :: [(String, [String])] -> [String]
findExpandables results =
  fmap fst (filter (\result -> length (snd result) == 10) results)

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left err -> (def, [])
    Right (key, vals) -> (key, vals)
