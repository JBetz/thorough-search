{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Search
  ( instasearch
  , thoroughSearch
  ) where

import           Config
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad.Catch    (catch)
import           Control.Monad.Reader
import           Data.Aeson             (Value(..), Array, eitherDecode)
import           Data.ByteString.Lazy   (ByteString)
import           Data.List              (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Maybe             (fromMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Database.SQLite.Simple (Connection)
import           Model
import           Network.HTTP.Client    (HttpException)
import           Network.Wreq
import           Storage

type Search = ReaderT SearchConfig IO

thoroughSearch :: Query -> Connection -> Int -> Search Int
thoroughSearch query conn maximumExpansionLength = do
  maxQueryCount <- asks search_maxQueryCount
  queryCount <- expansiveInstasearch query conn maximumExpansionLength
  if queryCount < maxQueryCount
    then thoroughSearch query conn (maximumExpansionLength + 1)
    else pure queryCount

expansiveInstasearch :: Query -> Connection -> Int -> Search Int
expansiveInstasearch q@(Query _ expansion _) conn maximumExpansionLength =
  if length expansion <= maximumExpansionLength
    then do
      results <- traverse (\expandedQuery -> instasearchWithRetry expandedQuery conn) (expandQuery q)
      recResultCounts <- traverse (\nq -> expansiveInstasearch nq conn maximumExpansionLength) (findExpandables results)
      pure $ length results + sum recResultCounts
    else pure 0

instasearchWithRetry :: Query -> Connection -> Search (Query, Int)
instasearchWithRetry q conn  = do
  retryDelay <- asks search_retryDelay
  catch
    (instasearchWithCache q conn)
    (\e -> do
       liftIO $ putStrLn $ show (e :: HttpException)
       liftIO $ secondsThreadDelay retryDelay
       instasearchWithCache q conn)

instasearchWithCache :: Query -> Connection -> Search (Query, Int)
instasearchWithCache q conn = do
  instasearchDelay <- asks search_instasearchDelay
  alreadyRan <- liftIO $ ranQuery q conn
  if alreadyRan
    then do
      resultCount <- liftIO $ selectQueryResultCount q conn
      pure (q, resultCount)
    else liftIO $ do
      msThreadDelay instasearchDelay
      results <- instasearch q
      _ <- insertResultList (q, results) conn
      putStrLn $ show q <> " - " <> show (length results)
      pure (q, length results)

instasearch :: Query -> IO [String]
instasearch q = do
  let opts = defaults & param "q" .~ [Text.pack $ show q] & param "client" .~ ["firefox"] & param "hl" .~ ["en"]
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
  case eitherDecode response :: Either String Value of
    Left _ -> []
    Right (Array vector) -> 
      case vector Vector.!? 1 of
        Just (Array results) -> mapMaybe (\case
            String text -> Just $ Text.unpack text
            _ -> Nothing ) (Vector.toList results)
        _ -> []

msThreadDelay :: Int -> IO ()
msThreadDelay ms = threadDelay $ ms * 1000

secondsThreadDelay :: Int -> IO ()
secondsThreadDelay seconds = threadDelay $ seconds * 1000 * 1000
