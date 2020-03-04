{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module DataSet
  ( readDataSet
  ) where

import Control.Exception
import Data.Char

data Class
  = Crew
  | First
  | Second
  | Third
  deriving (Show)

data Sex
  = Male
  | Female
  deriving (Show)

data Age
  = Adult
  | Child
  deriving (Show)

data Survived
  = Yes
  | No
  deriving (Show)

data Sample
  = Sample Class Sex Age Survived
  deriving (Show)

type HasHeader = Bool

data DataSetException
  = FormatError
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Read the input file and extract the data in an ADT.
--
-- The input should have the following form:
--
--   Class Sex Age Survived
--   Class:Crew Sex:Male Age:Adult Survived:No
--   Class:Crew Sex:Male Age:Adult Survived:No
--   ...
--
-- Otherwise it will throw an IOException.
readDataSet :: FilePath -> HasHeader -> IO [Sample]
readDataSet fp hasHeader = do
  content <- readFile fp
  let removeHeader = if hasHeader then tail else id
      lines' = removeHeader $ lines content
      total = length lines'
      (missingCount, samples) = foldr go (0, []) (lines')
  if missingCount > floor (fromIntegral(20*total)/100 :: Double)
    then throwIO FormatError
    else return samples

  where
    go :: String -> (Int, [Sample]) -> (Int, [Sample])
    go s (missing, acc) = case parseSample s of
      Just sample -> (missing, sample : acc)
      Nothing     -> (missing + 1, acc)


parseSample :: String -> Maybe Sample
parseSample s =
  case words s of
    [classS, sexS, ageS, survivedS] ->
      do claz     <- parseClass classS
         sex      <- parseSex sexS
         age      <- parseAge ageS
         survived <- parseSurvived survivedS
         return (Sample claz sex age survived)
    _ ->
      Nothing

parseClass :: String -> Maybe Class
parseClass s = case dropPrefix s of
  "Crew" -> Just Crew
  "1st"  -> Just First
  "2nd"  -> Just Second
  "3rd"  -> Just Third
  _      -> Nothing

parseSex :: String -> Maybe Sex
parseSex s = case (fmap toLower (dropPrefix s)) of
  "female" -> Just Female
  "male"   -> Just Female
  _        -> Nothing

parseAge :: String -> Maybe Age
parseAge s = case dropPrefix s of
  "Child" -> Just Child
  "Adult" -> Just Adult
  _       -> Nothing

parseSurvived :: String -> Maybe Survived
parseSurvived s = case dropPrefix s of
  "Yes" -> Just Yes
  "No"  -> Just No
  _     -> Nothing


sep :: Char
sep = ':'

dropPrefix :: String -> String
dropPrefix = tail . dropWhile (/= sep)
