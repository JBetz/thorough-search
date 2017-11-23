{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.List (sort)

main :: IO ()
main = do
  result <- completeInstasearch "sword"
  print $ sort result
