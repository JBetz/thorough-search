{-# LANGUAGE OverloadedStrings #-}

module Search
  ( instasearch
  , thoroughSearch
  ) where

import           Config
import           Control.Concurrent     (forkIO)
import           Control.Lens
import           Control.Monad.Catch    (catch)
import           Control.Monad.Reader
import           Data.Aeson             (eitherDecode)
import           Data.ByteString.Lazy   (ByteString)
import           Data.List              (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Text              (pack)
import           Database.SQLite.Simple (Connection)
import           Model
import           Network.HTTP.Client    (HttpException)
import           Network.Wreq
import           Storage

type Search = ReaderT SearchConfig IO

thoroughSearch :: Query -> Connection -> Int -> Search Int
thoroughSearch q conn mel = do
  rt <- asks maxRuntime
  queryCount <- expansiveInstasearch q conn mel
  if queryCount < rt * 1000
    then thoroughSearch q conn (mel + 1)
    else pure queryCount

expansiveInstasearch :: Query -> Connection -> Int -> Search Int
expansiveInstasearch q@(Query _ e _) conn mel =
  if length e <= mel
    then do
      results <- traverse (\eq -> instasearchWithRetry eq conn) (expandQuery q)
      recResultCounts <- traverse (\nq -> expansiveInstasearch nq conn mel) (findExpandables results)
      pure $ length results + sum recResultCounts
    else pure 0

instasearchWithRetry :: Query -> Connection -> Search (Query, Int)
instasearchWithRetry q conn  = do
  rd <- asks retryDelay
  catch
    (instasearchWithCache q conn)
    (\e -> do
       liftIO $ putStrLn $ show (e :: HttpException)
       liftIO $ secondsThreadDelay rd
       instasearchWithCache q conn)

instasearchWithCache :: Query -> Connection -> Search (Query, Int)
instasearchWithCache q conn = do
  isd <- asks instasearchDelay
  alreadyRan <- liftIO $ ranQuery q conn
  if alreadyRan
    then do
      resultCount <- liftIO $ selectQueryResultCount q conn
      pure (q, resultCount)
    else do
      liftIO $ msThreadDelay isd
      results <- liftIO $ instasearch q
      _ <- liftIO $ forkIO $ insertResultList (q, results) conn
      pure (q, length results)

instasearch :: Query -> IO [String]
instasearch q = do
  print $ show q
  let opts = defaults & param "q" .~ [pack $ show q] & param "client" .~ ["firefox"] & param "hl" .~ ["en"]
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
    Left _          -> []
    Right (_, vals) -> vals
