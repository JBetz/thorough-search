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

recursiveInstasearch :: Query -> Int -> Connection -> (FilterConfig, SearchConfig) -> IO Int
recursiveInstasearch q maxQueryLength conn cfg@(fcfg, scfg@(SearchConfig mfs _ _)) = do
  -- run with next max query length
  printInfo $ "running with max query length " ++ show maxQueryLength
  curResultCount <- recursiveInstasearch' q maxQueryLength conn scfg
  -- get current count
  curResults <- selectUniqueResults q conn
  curFilteredResults <- liftIO $ filterResults q 4 (filter (matches q) (fmap snd curResults)) fcfg
  let curFilteredResultCount = length $ concatMap _results curFilteredResults
  printStats $ show curFilteredResultCount ++ " filtered results"
  -- recurse if there's more to find
  if curResultCount == 0 || curFilteredResultCount > mfs
    then pure curResultCount
    else do
      nextResultCount <- recursiveInstasearch q (maxQueryLength + 1) conn cfg
      pure $ curResultCount + nextResultCount

recursiveInstasearch' :: Query -> Int -> Connection -> SearchConfig -> IO Int
recursiveInstasearch' q maxQueryLength conn cfg = do
  results <- traverse (\eq -> instasearchWithRetry eq conn cfg) (expandQuery q)
  let newQueries = findExpandables results maxQueryLength
  let resultCount = sum $ fmap (length . snd) results
  recResults <- traverse (\nq -> recursiveInstasearch' nq maxQueryLength conn cfg) newQueries
  pure $ resultCount + sum recResults

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

findExpandables :: [(Query, [String])] -> Int -> [Query]
findExpandables queries maxQueryLength =
  fmap
    fst
    (filter
       (\(q, results) ->
          length (expansion q) < maxQueryLength && length results == 10)
       queries)

parseResponse :: ByteString -> [String]
parseResponse response =
  case eitherDecode response :: Either String (String, [String]) of
    Left _ -> []
    Right (_, vals) -> vals

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000