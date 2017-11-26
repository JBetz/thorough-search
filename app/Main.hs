{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)
import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let query    = head args
  result <- recursiveAutocomplete query
  let filePath = "./output/" ++ query ++ ".txt"
  writeFile filePath (query ++ ": " ++ show (length result) ++ " results\n\n")
  sequence_ $ fmap (\r -> appendFile filePath (r ++ "\n")) result
