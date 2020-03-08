{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards       #-}

------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Description :  Includes all operations to run your database against Naive Bayes.
-- Copyright   :  2020 Arnau Abella
-- License     :  MIT
-- Maintainer  :  arnauabella@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Lib exports all modules required to train and predict using Naive Bayes.
--
-- Also includes a bunch of runners (e.g. @runNaiveBayes@, @runDummy@).
-- Runners train,run and test (using cross validation) a classifier.
-- The result of the test is ouputed to the stdin.
module Lib (
    -- * Modules
    -- ** Distribution
    module Distrib
    -- ** Classifier
  , module Classifier
    -- ** Validation
  , module Validation
    -- ** Parser
  , module Parser.Csv
    -- * Runners
  , runDummy
  , runNaiveBayes
  ) where

----------------------------------------------------------------

import           Classifier
import           Classifier.Dummy
import           Classifier.NaiveBayes
import           Data.List             (nub)
import           Data.Proxy
import           Distrib
import           Parser.Csv
import           System.FilePath.Posix (takeBaseName)
import           Validation

----------------------------------------------------------------

-- | Given the:
--
--   * a classifier
--   * the filepath of a dataset
--   * a function that points to the columns of the feature to label
--   * and a variable that describe if the data contains a header or not
--
-- Trains a /dummy/ (random) classifier and test it using /cross-validation/.
--
-- @
-- runDummy ("./datasets/wine-quality.csv", (\n -> n - 1), HasHeader)
-- @
runDummy :: (FilePath, (Int -> Classifier.Index), HasHeader) -> IO ()
runDummy = runClassifier (Proxy @Dummy)

-- | Given the:
--
--   * a classifier
--   * the filepath of a dataset
--   * a function that points to the columns of the feature to label
--   * and a variable that describe if the data contains a header or not
--
-- Trains a Naive Bayes Classifier and test it using /cross-validation/.
--
-- @
-- runNaiveBayes ("./datasets/wine-quality.csv", (\n -> n - 1), HasHeader)
-- @
runNaiveBayes :: (FilePath, (Int -> Classifier.Index), HasHeader) -> IO ()
runNaiveBayes = runClassifier (Proxy @NaiveBayes)

-------------------------------

-- | Generic runClassifier.
runClassifier
    :: forall c. Classifier.Classifier c
    => Proxy c
    -> (FilePath, (Int -> Classifier.Index), HasHeader)
    -> IO ()
runClassifier _ (datasetPath, getIndex, hasHeader) =
  printError $ do
    vec <- parseCsvFile @Sample datasetPath hasHeader
    samplesAndClasses <-
      liftEither $ traverse (\sample -> extractClass (getIndex (nFeatures sample)) sample) vec
    liftEither $ validateSamples samplesAndClasses
    let score = crossValidation samplesAndClasses (Proxy @c)
    liftIO $ report datasetPath samplesAndClasses score
  where
    nFeatures Sample{..} = length _features

-- | Print error.
printError :: (MonadIO m, Show e) => ExceptT e m () -> m ()
printError m = do
  r <- runExceptT m
  case r of
    Left err -> liftIO $ print ("An error occured: " ++ show err)
    Right _  -> return ()

-- | Reports the score of the cross-validation test
report :: FilePath -> [(Sample, Class)] -> Score -> IO ()
report fp samples score = do
  let classes = nub $ fmap snd samples
  putStrLn $ "---------" ++ "Dataset: " ++ takeBaseName fp
  putStrLn $ "#Classes: " ++ show (length classes)
  putStrLn $ "#Total of samples: " ++ show (length samples)
  print score
  putStrLn "--------------------------------------------"
