{-# LANGUAGE OverloadedStrings #-}

module Instasearch
  ( instasearch
  , recursiveInstasearch
  ) where

import           Config
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch  (catch)
import           Control.Monad.Reader
import           Data.Aeson           (eitherDecode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (pack)
import           Network.HTTP.Client  (HttpException)
import           Network.Wreq
import           Scowl
import           Storage

recursiveInstasearch :: String -> Int -> App Int
recursiveInstasearch query maxQueryLength = do
  (Config bq _) <- ask
  liftIO $ print $ "running with max query length " ++ show maxQueryLength
  currentResults <- recursiveInstasearch' query maxQueryLength
  allResults <- selectUniqueResults
  filteredResults <- liftIO $ filterResults bq allResults S35
  let filteredResultCount = (length . join) filteredResults
  liftIO $ print $ show filteredResultCount ++ " filtered results"
  if filteredResultCount > 500
    then pure currentResults
    else do
      nextResults <- recursiveInstasearch query (maxQueryLength + 1)
      pure $ currentResults + nextResults

recursiveInstasearch' :: String -> Int -> App Int
recursiveInstasearch' query maxQueryLength = do
  results <- traverse instasearchWithRetry (expandQuery query)
  let newQueries = findExpandables results maxQueryLength
  recResults <- traverse (`recursiveInstasearch'` maxQueryLength) newQueries
  pure $ length results + sum recResults

instasearchWithRetry :: String -> App (String, [String])
instasearchWithRetry query =
  catch
    (instasearchWithCache query)
    (\e -> do
       liftIO $ print (e :: HttpException)
       liftIO $ secondsThreadDelay 300
       instasearchWithCache query)

instasearchWithCache :: String -> App (String, [String])
instasearchWithCache query = do
  alreadyRan <- ranQuery query
  if alreadyRan
    then selectQueryResults query
    else do
      results <- instasearchWithRetry query
      _ <- insertResultList results
      pure results


instasearch :: String -> App (String, [String])
instasearch query = do
  liftIO $ print query
  let opts = defaults & param "q" .~ [pack query] & param "client" .~ ["firefox"]
  response <- liftIO $ getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) query

expandQuery :: String -> [String]
expandQuery bq =
  fmap (snoc bq) ['a'..'z']

findExpandables :: [(String, [String])] -> Int -> [String]
findExpandables queries maxQueryLength =
  fmap fst
    (filter
       (\(query, results) ->
        length (last (words query)) < maxQueryLength && length results == 10
       )
       queries)

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left _            -> (def, [])
    Right (key, vals) -> (key, vals)

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000
