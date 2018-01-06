{-# LANGUAGE OverloadedStrings #-}

module Search
  ( instasearch
  , recursiveInstasearch
  ) where

import Config
import Control.Concurrent
import Control.Lens
import Control.Monad.Catch (catch)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Text (pack)
import Database.SQLite.Simple (Connection)
import Model
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Storage

recursiveInstasearch :: Query -> Connection -> SearchConfig -> IO Int
recursiveInstasearch q@(Query _ e _) conn cfg@(SearchConfig mql _ _) =
  if length e <= mql 
    then do 
      print $ show q
      results <- traverse (\eq -> instasearchWithRetry eq conn cfg) (expandQuery q)
      let newQueries = findExpandables results
      recResults <- traverse (\nq -> recursiveInstasearch nq conn cfg) newQueries
      pure $ sum (fmap snd results) + sum recResults
    else pure 0

instasearchWithRetry :: Query -> Connection -> SearchConfig -> IO (Query, Int)
instasearchWithRetry q conn cfg@(SearchConfig _ _ rd) =
  catch
    (instasearchWithCache q conn cfg)
    (\e -> do
       putStrLn $ show (e :: HttpException)
       secondsThreadDelay rd
       instasearchWithCache q conn cfg)

instasearchWithCache :: Query -> Connection -> SearchConfig -> IO (Query, Int)
instasearchWithCache q conn(SearchConfig _ isd _) = do
  alreadyRan <- ranQuery q conn
  if alreadyRan
    then selectQueryResultCount q conn
    else do
      secondsThreadDelay isd
      results <- instasearch q
      rc <- insertResultList (q, results) conn
      pure (q, rc)

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

findExpandables :: [(Query, Int)] -> [Query]
findExpandables queries =
  fmap fst (filter (\q -> snd q == 10) queries)

parseResponse :: ByteString -> [String]
parseResponse response =
  case eitherDecode response :: Either String (String, [String]) of
    Left _ -> []
    Right (_, vals) -> vals

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000