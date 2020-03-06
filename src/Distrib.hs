{-# LANGUAGE GADTs #-}
module Distrib (
    -- * Data Constructors
    Distrib(..)
    -- * Operations
  , compNormalDistr
  , pdf
  ) where

------------------------------------------------------------------

import           Classifier
import qualified Data.Vector.Unboxed            as VU
import           Statistics.Distribution
import           Statistics.Distribution.Normal
import qualified Statistics.Sample              as Statistics

------------------------------------------------------------------

data Distrib where
  Distrib :: ContDistr d => d -> Distrib

-- | We don't know much about our numeric variable
-- so we will assume normallity to simplify the implementation.
compNormalDistr :: [Double] -> Distrib
compNormalDistr xs =
  Distrib (normalDistr mean' stdDev')
    where
      v = VU.fromList xs
      mean' = Statistics.mean v
      stdDev'  = max 0.1 (Statistics.stdDev v) -- stdDev must be > 0

-- | Probability Density Function
pdf :: Distrib -> Double -> Probability
pdf (Distrib d) = Probability . density d
