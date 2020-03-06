{-# LANGUAGE LambdaCase       #-}
module Main where

-------------------------------

import           Data.Foldable (traverse_)
import qualified Lib

-------------------------------

main :: IO ()
main =
  traverse_ Lib.runNaiveBayes [ ("./datasets/wine-quality.csv", (\n -> n - 1), Lib.HasHeader)
                              , ("./datasets/wine-origin.csv" , (\_ -> 0)    , Lib.NoHeader )
                              ]
