{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Validation
  ( module Validation
  ) where

import           Classifier
import           Data.Foldable
import           Data.Proxy
import           Types

-- | Accuracy score of a classifier.
data Score = Score
  { hit   :: Int
  , miss  :: Int
  , total :: Int
  }
  deriving Show

instance Semigroup Score where
  (Score h1 m1 t1) <> (Score h2 m2 t2) =
    Score (h1 + h2) (m1+m2) (t1+t2)

instance Monoid Score where
  mempty = Score 0 0 0

--instance Show Score where
  --show score =
    --"Score: " ++ show (getAccuracy score * 100) ++ " % accuracy"

hit' :: Score
hit' = mempty{ hit = 1, total = 1}

miss' :: Score
miss' = mempty{ miss = 1, total = 1}

getAccuracy :: Score -> Double
getAccuracy Score{..} = fromIntegral(hit) / fromIntegral(total)

type TrainingSet = [(Sample, Class)]

type TestSet = [(Sample, Class)]

crossValidation
    :: forall c. Classifier c
    => [(Sample, Class)] -- ^ All Samples
    -> Proxy c
    -> Score
crossValidation allSamples _ =
  let groups = leaveOneOutN (groupBy' 10 allSamples)
  in foldMap test groups
    where
      test (trainingSet, testSet) =
        let classifier = (train trainingSet) :: c
         in foldMap (predictAndGetScore classifier) testSet

      predictAndGetScore classifier (sample, clazz) =
        let predictions = predict sample classifier
            cmp = (\(_, p1) (_, p2) -> (_probability p1) `compare` (_probability p2))
            (pClazz, _) = maximumBy cmp predictions
         in
            if pClazz == clazz then hit' else miss'


-- | Generate at least n groups
groupBy' :: Int -> [a] -> [[a]]
groupBy' n xs = go [] [] xs
  where
    m = (length xs) `div` n

    go acc curr [] = curr : acc
    go acc curr (a:as)
      | length curr >= m = go (curr : acc) [] (a:as)
      | otherwise        = go acc (a : curr) as

leaveOneOutN :: [[(Sample, Class)]] -> [(TrainingSet, TestSet)]
leaveOneOutN xxs = go [] [] xxs where
  go acc _ []     = acc
  go acc l (x:xs) =
    let training = concat (l ++ xs)
        test = x
        l' = l ++ [x]
     in go ((training,test):acc) l' xs
