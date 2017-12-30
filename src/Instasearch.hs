{-# LANGUAGE OverloadedStrings #-}

module Instasearch
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
import Data.List (isSuffixOf)
import Data.Text (pack)
import Filter
import Model
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Storage

recursiveInstasearch :: Query -> Int -> App Int
recursiveInstasearch q maxQueryLength = do
  (Config bq _) <- ask
  -- get previous count
  prevAllResults <- selectUniqueResults
  prevFilteredResults <- liftIO $ filterResults bq 4 (fmap snd prevAllResults)
  let prevFilteredResultCount = length $ concatMap _results prevFilteredResults
  -- run with next max query length
  liftIO $ printInfo $ "running with max query length " ++ show maxQueryLength
  curResults <- recursiveInstasearch' q maxQueryLength
  -- get current count
  curAllResults <- selectUniqueResults
  curFilteredResults <- liftIO $ filterResults bq 4 (fmap snd curAllResults)
  let curFilteredResultCount = length $ concatMap _results curFilteredResults
  liftIO $ printStats $ show curFilteredResultCount ++ " filtered results"
  -- recurse if there's more to find
  if curFilteredResultCount > 1500 || curFilteredResultCount == prevFilteredResultCount
    then pure curResults
    else do
      nextResults <- recursiveInstasearch q (maxQueryLength + 1)
      pure $ curResults + nextResults

recursiveInstasearch' :: Query -> Int -> App Int
recursiveInstasearch' q maxQueryLength = do
  results <- traverse instasearchWithRetry (expandQuery q)
  let newQueries = findExpandables results maxQueryLength
  let resultCount = sum $ fmap (length . snd) results
  recResults <- traverse (`recursiveInstasearch'` maxQueryLength) newQueries
  pure $ resultCount + sum recResults

instasearchWithRetry :: Query -> App (Query, [String])
instasearchWithRetry q =
  catch
    (instasearchWithCache q)
    (\e -> do
       liftIO $ putStrLn $ show (e :: HttpException)
       liftIO $ secondsThreadDelay 300
       instasearchWithCache q)

instasearchWithCache :: Query -> App (Query, [String])
instasearchWithCache q = do
  alreadyRan <- ranQuery q
  if alreadyRan
    then selectQueryResults q
    else do
      liftIO $ print $ show q
      results <- liftIO $ instasearch q
      _ <- insertResultList (q, results)
      pure (q, results)

instasearch :: Query -> IO [String]
instasearch q = do
  let opts = defaults & param "q" .~ [pack $ show q] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) q

expandQuery :: Query -> [Query]
expandQuery (Query b e s)  = 
  let expandWith char = Query b (e ++ [char]) s
      invalid (Query _ e' _) = "  " `isSuffixOf` e' || head e' == ' '
  in filter (not . invalid) (fmap expandWith (' ' : ['a' .. 'z']))

findExpandables :: [(Query, [String])] -> Int -> [Query]
findExpandables queries maxQueryLength =
  fmap
    fst
    (filter
       (\(q, results) ->
          length (expansion q) < maxQueryLength && length results == 10)
       queries)

parseResponse :: ByteString -> Query -> [String]
parseResponse response q =
  case eitherDecode response :: Either String (String, [String]) of
    Left _ -> []
    Right (_, vals) -> vals

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000