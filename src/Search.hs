{-# LANGUAGE OverloadedStrings #-}

module Search
  ( instasearch
  , recursiveInstasearch
  ) where

import Config
import Control.Concurrent
import Control.Lens
import Control.Monad.Catch (catch)
import Control.Monad.Reader
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Text (pack)
import Database.SQLite.Simple (Connection)
import Filter
import Model
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Storage

recursiveInstasearch :: Query -> Connection -> (FilterConfig, SearchConfig) -> IO Int
recursiveInstasearch q conn cfg@(fcfg, scfg) = do
  curResultCount <- recursiveInstasearch' q conn scfg
  -- get current count
  curResults <- selectUniqueResults q conn
  curFilteredResults <- liftIO $ filterResults q (filter (matches q) (fmap snd curResults)) fcfg
  let curFilteredResultCount = length $ concatMap _results curFilteredResults
  printStats $ show curFilteredResultCount ++ " filtered results"
  -- recurse if there's more to find
  if curResultCount == 0
    then pure curResultCount
    else do
      nextResultCount <- recursiveInstasearch q conn cfg
      pure $ curResultCount + nextResultCount

recursiveInstasearch' :: Query -> Connection -> SearchConfig -> IO Int
recursiveInstasearch' q@(Query _ e _) conn cfg@(SearchConfig mql _ _) =
  if length e < mql 
    then do 
      results <- traverse (\eq -> instasearchWithRetry eq conn cfg) (expandQuery q)
      let newQueries = findExpandables results
      let resultCount = sum $ fmap (length . snd) results
      recResults <- traverse (\nq -> recursiveInstasearch' nq conn cfg) newQueries
      pure $ resultCount + sum recResults
    else pure 0

instasearchWithRetry :: Query -> Connection -> SearchConfig -> IO (Query, [String])
instasearchWithRetry q conn cfg@(SearchConfig _ _ rd) =
  catch
    (instasearchWithCache q conn cfg)
    (\e -> do
       putStrLn $ show (e :: HttpException)
       secondsThreadDelay rd
       instasearchWithCache q conn cfg)

instasearchWithCache :: Query -> Connection -> SearchConfig -> IO (Query, [String])
instasearchWithCache q conn(SearchConfig _ isd _) = do
  alreadyRan <- ranQuery q conn
  if alreadyRan
    then selectQueryResults q conn
    else do
      print $ show q
      secondsThreadDelay isd
      results <- instasearch q
      _ <- insertResultList (q, results) conn
      pure (q, results)

instasearch :: Query -> IO [String]
instasearch q = do
  let opts = defaults & param "q" .~ [pack $ show q] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search" 
  pure $ parseResponse (response ^. responseBody)

expandQuery :: Query -> [Query]
expandQuery (Query b e s)  = 
  let expandWith char = Query b (e ++ [char]) s
      invalid q = "  " `isPrefixOf` sq || "  " `isSuffixOf` sq || "   " `isInfixOf` sq || head sq == ' '
        where sq = show q
  in filter (not . invalid) (fmap expandWith (' ' : ['a' .. 'z']))

findExpandables :: [(Query, [String])] -> [Query]
findExpandables queries =
  fmap fst
    (filter (\q -> (length . snd) q == 10) queries)

parseResponse :: ByteString -> [String]
parseResponse response =
  case eitherDecode response :: Either String (String, [String]) of
    Left _ -> []
    Right (_, vals) -> vals

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000