{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Classifier
import           Data.List  (nub)
import           Data.Proxy
import           Parser.Csv
import           Types
import           Validation

main :: IO ()
main = runClassifier


runClassifier :: IO ()
runClassifier = run $ do
  vec <- parseCsvFile @Sample "./datasets/winequality-red.csv" HasHeader
  liftIO $ print ("#Samples: " ++ show (length vec))
  samples <-
    liftEither $
      traverse (\sample -> extractClass ((nFeatures sample) - 1) sample) vec
  liftIO $ printNClasses samples
  let score = crossValidation (take 20 samples) (Proxy @Dummy)
  liftIO $ do
    putStrLn ""
    putStrLn "---- Dummy Classifier"
    print score
    putStrLn ""


run :: (MonadIO m, Show e) => ExceptT e m () -> m ()
run m = do
  r <- runExceptT m
  case r of
    Left err -> liftIO $ print ("An error occured: " ++ show err)
    Right _  -> return ()


printNClasses :: [(Sample, Class)] -> IO ()
printNClasses xs = do
  let classes = nub $ fmap snd xs
  putStrLn $ "#Classes: " ++ show (length classes)

