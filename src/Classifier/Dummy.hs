{-# LANGUAGE GADTs           #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Classifier.Dummy (
    Dummy(..)
  ) where

import           Classifier
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random    as R

--------------------------------------------

data Dummy = Dummy
  { _classes :: Set Class
  }

instance Classifier Dummy where
  train :: [(Sample, Class)] -> Dummy
  train trainingSet =
    Dummy $ Set.fromList $ fmap (\(_, clazz) -> clazz) trainingSet
  {-# INLINE train #-}

  predict :: Sample -> Dummy -> [(Class, Probability)]
  predict _ Dummy{..} =
    let p = unsafePerformIO (R.getStdRandom R.random :: IO Double) -- [0,1)
        winner = Set.elemAt (floor (p * fromIntegral (Set.size _classes))) _classes
        remaining = fmap (, Probability 0) $ Set.toList (Set.delete winner _classes)
     in (winner, Probability 1.0):remaining
  {-# INLINE predict #-}

