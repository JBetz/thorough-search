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
import           Data.List            (isSuffixOf, sort, union)
import           Data.Pool
import qualified Data.Set             as S
import           Data.Text            (pack)
import           Database.Persist.Sql (Key, SqlBackend, count, insertUnique,
                                       runSqlPool, (==.))
import           Network.HTTP.Client  (HttpException)
import           Network.Wreq
import           Persistence

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

recursiveInstasearch :: String -> App [Maybe (Key Result)]
recursiveInstasearch query = do
  liftIO $ print query
  config <- ask
  alreadyRan <- ranQuery query
  if alreadyRan
    then pure []
    else do
      results <-
        liftIO $ sequence $ fmap instasearchWithRetry (expandQuery query)
      recResults <-
        sequence $ fmap recursiveInstasearch (findExpandables results)
      dbResults <- traverse insertResult results
      pure $ join dbResults `union` join recResults

instasearchWithRetry :: String -> IO (String, [String])
instasearchWithRetry query =
  catch
    (instasearch query)
    (\e -> do
       print (e :: HttpException)
       msThreadDelay 30000
       instasearch query)

instasearch :: String -> IO (String, [String])
instasearch query = do
  let opts =
        defaults & param "q" .~ [pack query] & param "client" .~ ["firefox"]
  response <- getWith opts "https://www.google.com/complete/search"
  pure $ parseResponse (response ^. responseBody) query

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
    Left err          -> (def, [])
    Right (key, vals) -> (key, vals)

msThreadDelay :: Int -> IO ()
msThreadDelay ms = threadDelay $ ms * 1000
