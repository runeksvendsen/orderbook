module Orphans.Market where

import MyPrelude
import Lib.Markets.Types
import qualified Test.SmallCheck.Series as SS
import Data.Ord


newtype HyphenChar = HyphenChar {hcGet :: Char}
newtype HyphenStr = HyphenStr String

instance Show HyphenStr where
   show (HyphenStr str) = show str

instance Monad m => SS.Serial m HyphenChar where
   series = HyphenChar <$> SS.generate (\d -> take (d+1) ['a','-','c','d'])

instance Monad m => SS.Serial m HyphenStr where
   series = do
      SS.NonEmpty str :: SS.NonEmpty HyphenChar <- SS.series
      return $ HyphenStr (map hcGet str)

--instance Monad m => SS.CoSerial m Char where
--  coseries rs =
--    coseries rs >>- \f ->
--    return $ \c -> f (N (fromEnum c - fromEnum 'a'))


instance Monad m => SS.Serial m AnyMarket where
      series = do
         SS.NonEmpty base  :: SS.NonEmpty Char <- SS.series
         SS.NonEmpty quote :: SS.NonEmpty Char <- SS.series
         HyphenStr sym <- SS.series
         let market = Market (toS base) (toS quote) (toS sym) :: Market "testVenue"
         return (AnyMarket market)
