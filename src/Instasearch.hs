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
import           Data.List            (union)
import           Data.Text            (pack)
import           Network.HTTP.Client  (HttpException)
import           Network.Wreq
import           Scowl
import           Storage

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

recursiveInstasearch :: String -> Int -> App [(String, [String])]
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
      pure $ currentResults ++ nextResults

recursiveInstasearch' :: String -> Int -> App [(String, [String])]
recursiveInstasearch' query maxQueryLength = do
  liftIO $ print query
  results <- traverse instasearchWithRetry (expandQuery query)
  let newQueries = findExpandables results maxQueryLength
  recResults <- traverse (`recursiveInstasearch'` maxQueryLength) newQueries
  pure $ results `union` join recResults

instasearchWithRetry :: String -> App (String, [String])
instasearchWithRetry query =
  catch
    (instasearch query)
    (\e -> do
       liftIO $ print (e :: HttpException)
       liftIO $ msThreadDelay 300000
       instasearch query)

instasearch :: String -> App (String, [String])
instasearch query = do
  alreadyRan <- ranQuery query
  if alreadyRan
    then selectQueryResults query
    else do
      let opts = defaults & param "q" .~ [pack query] & param "client" .~ ["firefox"]
      response <- liftIO $ getWith opts "https://www.google.com/complete/search"
      let results = parseResponse (response ^. responseBody) query
      dbResults <- insertResultList results
      pure results

expandQuery :: String -> [String]
expandQuery bq =
  fmap (snoc bq) alphabet

findExpandables :: [(String, [String])] -> Int -> [String]
findExpandables queries maxQueryLength =
  fmap fst
    (filter
       (\(query, results) ->
        length (last (words query)) <= maxQueryLength && length results == 10
       )
       queries)

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left _            -> (def, [])
    Right (key, vals) -> (key, vals)

msThreadDelay :: Int -> IO ()
msThreadDelay ms = threadDelay $ ms * 1000
