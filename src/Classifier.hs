{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Classifier where

--import Statistics.Distribution
--import Statistics.Distribution.Normal
import Types
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Random as R
import           System.IO.Unsafe (unsafePerformIO)

class Classifier c where
  -- | Given a training set returns a trained classifier
  train :: [(Sample, Class)] -> c
  -- | Given a random sample, predict a class
  predict :: Sample -> c -> [(Class, Probability)] -- ^ TODO ordered ?

-- # fit a probability distribution to a univariate data sample
-- def fit_distribution(data):
-- 	# estimate parameters
-- 	mu = mean(data)
-- 	sigma = std(data)
-- 	print(mu, sigma)
-- 	# fit distribution
-- 	dist = norm(mu, sigma)
-- 	return dist
--
-- # calculate the independent conditional probability
-- def probability(X, prior, dist1, dist2):
-- 	return prior * dist1.pdf(X[0]) * dist2.pdf(X[1])
--
-- # generate 2d classification dataset
-- X, y = make_blobs(n_samples=100, centers=2, n_features=2, random_state=1)
--
-- # sort data into classes
-- Xy0 = X[y == 0]
-- Xy1 = X[y == 1]
--
-- # calculate priors
-- priory0 = len(Xy0) / len(X)
-- priory1 = len(Xy1) / len(X)
--
-- # create PDFs for y==0
-- distX1y0 = fit_distribution(Xy0[:, 0])
-- distX2y0 = fit_distribution(Xy0[:, 1])
-- # create PDFs for y==1
-- distX1y1 = fit_distribution(Xy1[:, 0])
-- distX2y1 = fit_distribution(Xy1[:, 1])
--
-- # classify one example
-- Xsample, ysample = X[0], y[0]
-- py0 = probability(Xsample, priory0, distX1y0, distX2y0)
-- py1 = probability(Xsample, priory1, distX1y1, distX2y1)
-- print('P(y=0 | %s) = %.3f' % (Xsample, py0*100))
-- print('P(y=1 | %s) = %.3f' % (Xsample, py1*100))
-- print('Truth: y=%d' % ysample)

--------------------------------------------

data Dummy = Dummy
  { _classes :: Set Class
  }

instance Classifier Dummy where
  train :: [(Sample, Class)] -> Dummy
  train trainingSet = Dummy $ Set.fromList $ fmap (\(_, clazz) -> clazz) trainingSet

  {-# INLINE train #-}
  predict :: Sample -> Dummy -> [(Class, Probability)]
  predict _ Dummy{..} =
    let p = unsafePerformIO (R.getStdRandom R.random :: IO Double) -- [0,1)
        winner = Set.elemAt (floor (p * fromIntegral (Set.size _classes))) _classes
        remaining = fmap (, Probability 0) $ Set.toList (Set.delete winner _classes)
     in (winner, Probability 1.0):remaining

--------------------------------------------

data NaiveBayes = Int

instance Classifier NaiveBayes where
  train :: [(Sample, Class)] -> NaiveBayes
  train = undefined
  {-# INLINE train #-}

  predict :: Sample -> NaiveBayes -> [(Class, Probability)]
  predict = undefined
  {-# INLINE predict #-}




























