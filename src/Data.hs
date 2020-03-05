{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data where

import           Control.Applicative ((<|>))
import qualified Data.Csv            as Csv
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics

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
