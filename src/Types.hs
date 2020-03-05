{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types where

import           Control.Applicative ((<|>))
import qualified Data.Csv            as Csv
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics

-- TODO
type Error = String

-- | Sample feature
data Feature
  = Binary      Bool       -- ^ Binomial Naive Bayes
  | Categorical Text       -- ^ Multinomial Naive Bayes
  | Numeric     Double     -- ^ Gaussian Naive Bayes
  deriving (Show)

instance Csv.FromField Feature where
  parseField :: Csv.Field -> Csv.Parser Feature
  parseField f =  (Binary <$> Csv.parseField f)
              <|> (Categorical <$> Csv.parseField f)
              <|> (Numeric <$> Csv.parseField f)

newtype Sample = Sample { _features :: [Feature]  }
  deriving (Show, Generic)
  deriving newtype (Csv.FromRecord)

nFeatures :: Sample -> Int
nFeatures Sample{..} = length _features

-- | Class which the variable X belongs.
--
-- You usually want to classify your variables/samples in the given classes.
newtype Class = Class { label :: Text }
  deriving (Show, Ord, Eq)

-- | Extract the class in the index i from the features of the sample.
-- And returns, if everything goes well, the class and
-- the updated features w/o the class feature.
extractClass :: Int -> Sample -> Either Error (Sample, Class)
extractClass i Sample{..} = do
  let (l, r') = splitAt i _features
  feature <-
    if null r'
      then Left "Index out of Bounds"
      else Right (head r')
  let r       = tail r'
      sample' = Sample { _features = l ++ r }
  c <- featureToClass feature
  return (sample', c)

featureToClass :: Feature -> Either Error Class
featureToClass (Binary b) = Right $ Class (showText b)
featureToClass (Categorical c) = Right $ Class c
featureToClass (Numeric n) =
  if (fromIntegral ((floor n)::Int) == n)
    then Right $ Class (showText (floor n :: Int))
    else Left ("Don't use real values as categorical classes!")

-- | Probability in [0,1]
newtype Probability = Probability { _probability :: Double }
  deriving (Show)

-----------------------------------------------------

showText :: Show a => a -> Text
showText = Text.pack . show

------------------------------------------------------
-- Orphan instances.

-- | Bool can be represented in several ways in a CSV:
--    * "true","false"
--    * 0, 1
--
-- I will assume the former to simplify things
instance Csv.FromField Bool where
  parseField f = do
    t <- Csv.parseField @Text f
    case Text.toLower t of
      "true"  -> pure True
      "false" -> pure False
      _       -> fail "Not a boolean field."
