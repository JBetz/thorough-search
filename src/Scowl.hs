{-# LANGUAGE OverloadedStrings #-}

module Scowl
  ( loadWordsFromScowl
  , Size(..)
  ) where

import           Control.Monad
import           System.IO

data Size
  = S10
  | S20
  | S35
  | S40
  | S50
  | S55
  | S60
  | S70
  | S80
  | S95
  deriving (Read, Enum)

show :: Size -> String
show size =
  Prelude.show $
  case size of
    S10 -> 10
    S20 -> 20
    S35 -> 35
    S40 -> 40
    S50 -> 50
    S55 -> 55
    S60 -> 60
    S70 -> 70
    S80 -> 80
    S95 -> 95

loadWordsFromScowl :: Size -> IO [String]
loadWordsFromScowl size = do
  words <-
    traverse
      (\s -> loadWordsFromFile ("./scowl/final/english-words." ++ Scowl.show s))
      (enumFromTo S10 size)
  pure $ join words

loadWordsFromFile :: FilePath -> IO [String]
loadWordsFromFile path = do
  fileContents <- readFile path
  pure $ lines fileContents
