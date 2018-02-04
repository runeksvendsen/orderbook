{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Lib.Markets.Types where

import MyPrelude
import Lib.Fetch
import Lib.OrderBook.Types
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


-- | Use 'miApiSymbol' from 'Market' to fetch a 'SomeBook'
class KnownSymbol venue => MarketBook venue where
   marketBook :: Text -> DataSrc (SomeBook venue)

data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: Text
   } deriving (Eq, Generic)

newtype MarketList venue = MarketList [Market venue]

data AnyMarket = forall venue. MarketBook venue => AnyMarket (Market venue)

instance Eq AnyMarket where
   (AnyMarket (m1 :: Market venue1)) == (AnyMarket (m2 :: Market venue2)) =
      miBase m1 == miBase m2 &&
      miQuote m1 == miQuote m2 &&
      miApiSymbol m1 == miApiSymbol m2 &&
      isJust (sameSymbol (Proxy :: Proxy venue1) (Proxy :: Proxy venue2))

instance KnownSymbol venue => Show (Market venue) where
   show Market{..} = printf template venueName miBase miQuote
      where
         template = "<%s %s/%s>"
         venueName = symbolVal (Proxy :: Proxy venue)

instance Show AnyMarket where
   show (AnyMarket m) = show m
