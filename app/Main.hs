module Main where

import DataSet

main :: IO ()
main = do
  let hasHeader = True
      fp = "./titanicTr.txt"
  samples <- readDataSet fp hasHeader
  print samples
