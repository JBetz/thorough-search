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
import Data.List (isSuffixOf)
import Data.Set (Set, fromList, union, difference, empty, elems, notMember)
import Data.Text (pack)

alphabet :: String
alphabet =
  " abcdefghijklmnopqrstuvwzyz"

completeInstasearch :: String -> IO [String]
completeInstasearch search =
  runCompleteInstasearch search empty

runCompleteInstasearch :: String -> Set String -> IO [String]
runCompleteInstasearch query seen = do
  results <- sequence $ fmap instasearch (generateQueries query seen)
  let newSeen = seen `union` fromList (concatMap snd results)
  recursiveResults <- sequence $ fmap (`runCompleteInstasearch` newSeen) (filterResults results newSeen)
  pure $ concatMap snd results ++ concat recursiveResults

instasearch :: String -> IO (String, [String])
instasearch search = do
  threadDelay (1 * 1000000)
  let opts = defaults & param "q" .~ [pack search] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) search

generateQueries :: String -> Set String -> [String]
generateQueries baseQuery seen =
  let isValid q = notMember q seen && not ("  " `isSuffixOf` q)
  in filter isValid (fmap (snoc baseQuery) alphabet)

filterResults :: [(String, [String])] -> Set String -> [String]
filterResults results seen =
  let keys = fmap fst (filter (\result -> length (snd result) == 10) results)
  in elems $ difference (fromList keys) seen

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left err -> (def, [])
    Right (key, vals) -> (key, filter (\val -> length (words val) <= 3) vals)
