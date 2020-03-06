module Parser.Csv (
    parseCsvFile
    -- * Reexports
  , module Data.Csv
  , module Control.Monad.Except
  ) where

--------------------------------------------------------------

import           Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Csv                   (HasHeader (..))
import qualified Data.Csv                   as Csv
import qualified Data.Vector                as V
import qualified System.FilePath            as FP

--------------------------------------------------------------

parseCsvFile
    :: Csv.FromRecord a
    => FilePath -> HasHeader -> ExceptT String IO [a]
parseCsvFile fp hasHeader = do
  liftEither (checkExtension fp)
  lbs <- liftIO (LBSC.readFile fp)
  liftEither $ fmap V.toList (Csv.decode hasHeader lbs)

csvExtension :: String
csvExtension = ".csv"

checkExtension :: FilePath -> Either String ()
checkExtension fp =
  if FP.isExtensionOf csvExtension fp
    then Right ()
    else Left ("Invalid extension: " <> FP.takeExtension fp)
