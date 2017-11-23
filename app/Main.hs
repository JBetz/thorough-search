{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  result <- completeInstasearch "sealed" 2
  print result
