{-# LANGUAGE OverloadedStrings #-}

module Instasearch
  ( instasearch
  , recursiveInstasearch
  ) where

import Config
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.Reader
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack)
import Model
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Scowl
import Storage

recursiveInstasearch :: Query -> Int -> App Int
recursiveInstasearch q maxQueryLength = do
  (Config bq _) <- ask
  liftIO $ print $ "running with max query length " ++ show maxQueryLength
  currentResults <- recursiveInstasearch' q maxQueryLength
  allResults <- selectAllResultPairs
  filteredResults <- liftIO $ filterResults bq allResults S35
  let filteredResultCount = (length . join) filteredResults
  liftIO $ print $ show filteredResultCount ++ " filtered results"
  if filteredResultCount > 1000
    then pure currentResults
    else do
      nextResults <- recursiveInstasearch q (maxQueryLength + 1)
      pure $ currentResults + nextResults

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
       liftIO $ print (e :: HttpException)
       liftIO $ secondsThreadDelay 300
       instasearchWithCache q)

instasearchWithCache :: Query -> App (Query, [String])
instasearchWithCache q = do
  alreadyRan <- ranQuery q
  if alreadyRan
    then selectQueryResults q
    else do
      results <- instasearch q
      _ <- insertResultList results
      pure results

instasearch :: Query -> App (Query, [String])
instasearch q = do
  liftIO $ print q
  let opts =
        defaults & param "q" .~ [pack (show q)] & param "client" .~ ["firefox"]
  response <- liftIO $ getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) q

expandQuery :: Query -> [Query]
expandQuery (Query b e s)  = 
  let expandWith char = Query b (e ++ [char]) s
  in fmap expandWith ['a' .. 'z']

findExpandables :: [(Query, [String])] -> Int -> [Query]
findExpandables queries maxQueryLength =
  fmap
    fst
    (filter
       (\(q, results) ->
          length (expansion q) < maxQueryLength && length results == 10)
       queries)

parseResponse :: ByteString -> Query -> (Query, [String])
parseResponse response q =
  case eitherDecode response :: Either String (String, [String]) of
    Left _ -> (q, [])
    Right (_, vals) -> (q, vals)

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000