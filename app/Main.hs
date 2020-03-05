{-# LANGUAGE TypeApplications #-}
module Main where

import Parser.Csv
import Data

main :: IO ()
main = runClassifier

runClassifier :: IO ()
runClassifier = do
  r <- runExceptT $ do
          v <- parseCsvFile @Sample "./datasets/winequality-red.csv" HasHeader
          liftIO $ print v
  case r of
    Left err -> print ("An error occured: " ++ show err)
    Right _  -> return ()
