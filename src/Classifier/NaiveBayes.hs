{-# LANGUAGE GADTs           #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Classifier.NaiveBayes (
    NaiveBayes(..)
  ) where

import           Classifier
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Distrib

--------------------------------------------

data NaiveBayes = NaiveBayes
  { labels  :: [Class] -- ^ All labels
  , priors  :: Map Class Probability
  , distrib :: Map Class (Map Index Distrib)
  }

----------------------------------------------------

instance Classifier NaiveBayes where
  train :: [(Sample, Class)] -> NaiveBayes
  train = trainNaiveBayes
  {-# INLINE train #-}

  predict :: Sample -> NaiveBayes -> [(Class, Probability)]
  predict = predictNaiveBayes
  {-# INLINE predict #-}

-----------------------------------------------------


trainNaiveBayes :: [(Sample, Class)] -> NaiveBayes
trainNaiveBayes trainingSet =
  NaiveBayes
    { labels = classes
    , priors = classesProb
    , distrib = distrByClass featuresByClass
    }
  where
    classes         = fmap snd trainingSet
    classesProb     = probClass classes
    featuresByClass = featuresMap classes trainingSet
{-# INLINE trainNaiveBayes #-}

featuresMap :: [Class] -> [(Sample, Class)] -> Map Class (Map Index [Feature])
featuresMap classes = foldr go b
  where
    b = Map.fromList (zip classes (repeat (Map.empty :: Map Index [Feature])))
    go (Sample features, clazz) = Map.update (Just . f) clazz where
      f m = foldr (\(feature, i) -> Map.insertWith (++) i [feature]) m (zip features [1..])

-- TODO: implement distributions for Binary, Categorical features.
distrByClass :: Map Class (Map Index [Feature]) -> Map Class (Map Index Distrib)
distrByClass = (fmap . fmap) f where
  f features =
    case (head features) of
      Binary _      -> error "Not implemented yet :("
      Categorical _ -> error "Not implemented yet :("
      Numeric _     -> compNormalDistr (fmap (\(Numeric d) -> d) features)


predictNaiveBayes :: Sample -> NaiveBayes -> [(Class, Probability)]
predictNaiveBayes (Sample features) NaiveBayes{..} =
  fmap f labels
    where
      f clazz = let prob = foldr go (priors Map.! clazz) (zip features [1..])
                in (clazz, prob)
        where
          go (feature, i) acc = case feature of
            Binary _      -> error "Not implemented yet :("
            Categorical _ -> error "Not implemented yet :("
            Numeric value -> acc * pdf ((distrib Map.! clazz) Map.! i) value
{-# INLINE predictNaiveBayes #-}

-----------------------------------------------------

probClass :: [Class] -> Map Class Probability
probClass classes =
  let n  = length classes
      cClass = count classes
   in fmap (\k -> Probability (k `divNum` n)) cClass


count :: (Ord a) => [a] -> Map a Int
count = foldr go Map.empty
  where
    go a = Map.insertWith (+) a 1

divNum :: (Integral a, Fractional b) => a -> a -> b
divNum a a2 = fromIntegral a / fromIntegral a2
