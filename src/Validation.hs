{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Validation (
    Score(..)
  , crossValidation
  , validateSamples
  ) where

import           Classifier
import           Data.Foldable
import           Data.Proxy

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

--getAccuracy :: Score -> Double
--getAccuracy Score{..} = fromIntegral(hit) / fromIntegral(total)


hit' :: Score
hit' = mempty{ hit = 1, total = 1}

miss' :: Score
miss' = mempty{ miss = 1, total = 1}

type TrainingSet = [(Sample, Class)]

type TestSet = [(Sample, Class)]

-- | X-Validation testing algorithm
--
-- Notice that every subset of <training, test> is different and generates a new classifier,
-- so the score is not for a single classifier, but for many.
-- Let's assume this is ok to simplify things.
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


validateSamples :: [(Sample, Class)] -> Either Error ()
validateSamples samplesAndClasses = do
  let samples = fmap fst samplesAndClasses
      Sample features = head samples
      expectedLength = length features
   in forM_ (tail samples) $ \sample@(Sample features') -> do
         if length features' /= expectedLength
           then Left ("Length of features is different for sample: " ++ show sample)
           else Right ()
         if any (\(f1, f2) -> not (f1 `eqv` f2)) (zip features features')
           then Left ("Sample with not the same type of features: " ++ show sample)
           else Right ()

----------------------------------------------------------

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
