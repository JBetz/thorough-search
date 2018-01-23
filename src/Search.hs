{-# LANGUAGE OverloadedStrings #-}

module Search
  ( instasearch
  , thoroughSearch
  ) where

import Config
import Control.Concurrent (forkIO)
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

thoroughSearch :: Query -> Connection -> Int -> SearchConfig -> IO Int 
thoroughSearch q conn mel cfg@(SearchConfig rt _ _) = do 
  queryCount <- expansiveInstasearch q conn mel cfg
  if queryCount < rt * 60 * 60
    then thoroughSearch q conn (mel + 1) cfg
    else pure queryCount

expansiveInstasearch :: Query -> Connection -> Int -> SearchConfig -> IO Int
expansiveInstasearch q@(Query _ e _) conn mel cfg =
  if length e <= mel 
    then do 
      results <- traverse (\eq -> instasearchWithRetry eq conn cfg) (expandQuery q)
      recResultCounts <- traverse (\nq -> expansiveInstasearch nq conn mel cfg) (findExpandables results)
      pure $ length results + sum recResultCounts
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
instasearchWithCache q conn (SearchConfig _ isd _) = do
  alreadyRan <- ranQuery q conn
  if alreadyRan
    then do 
      resultCount <- selectQueryResultCount q conn
      pure (q, resultCount)
    else do
      msThreadDelay isd
      results <- instasearch q
      forkIO $ insertResultList (q, results) conn
      pure (q, length results)

instasearch :: Query -> IO [String]
instasearch q = do
  print $ show q
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