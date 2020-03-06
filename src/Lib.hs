{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards       #-}
module Lib (
    -- * Modules
    module Distrib
  , module Classifier
  , module Validation
  , module Parser.Csv
    -- * Runners
  , runDummy
  , runNaiveBayes
  , runClassifier
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

-- | Given the filepath of a dataset, a function that points to the columns of the feature to label
-- and a variable that describe if the data contains a header or not, trains a Dummy (random) classifier
-- and test it using the cross-validation technique.
runDummy :: (FilePath, (Int -> Classifier.Index), HasHeader) -> IO ()
runDummy = runClassifier (Proxy @Dummy)

-- | Given the filepath of a dataset, a function that points to the columns of the feature to label
-- and a variable that describe if the data contains a header or not, trains a Naive Bayes classifier
-- and test it using the cross-validation technique.
runNaiveBayes :: (FilePath, (Int -> Classifier.Index), HasHeader) -> IO ()
runNaiveBayes = runClassifier (Proxy @NaiveBayes)

-- | Given the tag for a classifier, the filepath of a dataset, a function that points to the columns of the feature to label
-- and a variable that describe if the data contains a header or not, trains the classifier and test it using the cross-validation technique.
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

-------------------------------

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
