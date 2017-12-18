{-# LANGUAGE OverloadedStrings #-}

module Instasearch
  ( instasearch
  , recursiveInstasearch
  ) where

import           Config
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson           (eitherDecode)
import           Data.ByteString.Lazy (ByteString)
import           Data.List            (isSuffixOf, union)
import           Data.Text            (pack)
import           Network.HTTP.Client  (HttpException)
import           Network.Wreq
import           Storage

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

recursiveInstasearch :: String -> App [()]
recursiveInstasearch query = do
  liftIO $ print query
  alreadyRan <- ranQuery query
  if alreadyRan
    then pure []
    else do
      results <- liftIO $ traverse instasearchWithRetry (expandQuery query)
      recResults <- traverse recursiveInstasearch (findExpandables results)
      dbResults <- traverse insertResultList results
      pure $ join dbResults `union` join recResults

instasearchWithRetry :: String -> IO (String, [String])
instasearchWithRetry query =
  catch
    (instasearch query)
    (\e -> do
       print (e :: HttpException)
       msThreadDelay 300000
       instasearch query)

instasearch :: String -> IO (String, [String])
instasearch query = do
  let opts =
        defaults & param "q" .~ [pack query] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) query

expandQuery :: String -> [String]
expandQuery bq =
  let isValid q = not ("  " `isSuffixOf` q)
  in filter isValid (fmap (snoc bq) alphabet)

findExpandables :: [(String, [String])] -> [String]
findExpandables results =
  fmap
    fst
    (filter
       (\r -> length (snd r) == 10 && length ((last . words . fst) r) <= 4)
       results)

parseResponse :: ByteString -> String -> (String, [String])
parseResponse response def =
  case eitherDecode response :: Either String (String, [String]) of
    Left _            -> (def, [])
    Right (key, vals) -> (key, vals)

msThreadDelay :: Int -> IO ()
msThreadDelay ms = threadDelay $ ms * 1000
