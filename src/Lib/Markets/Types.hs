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


class MarketInfo (venue :: Symbol) (base :: Symbol) (quote :: Symbol) where
   marketBook :: Market venue -> DataSrc (OrderBook venue base quote)

data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: Text
   }

newtype MarketList venue = MarketList [Market venue]

data AnyMarket = forall venue. (KnownSymbol venue) => AnyMarket (Market venue)

withMarketBook
   :: Market venue
   -> (forall base quote. (KnownSymbol base, KnownSymbol quote) => OrderBook venue base quote -> ret)
   -> ret
withMarketBook mk f =
   case someSymbolVal (toS $ miBase mk) of
      SomeSymbol (Proxy :: Proxy base) ->
         case someSymbolVal (toS $ miQuote mk) of
               SomeSymbol (Proxy :: Proxy quote) -> undefined
--                  f (Dense (someDenseAmount dr) :: OrderBook venue base quote)

instance KnownSymbol venue => Show (Market venue) where
   show Market{..} = printf template venueName miBase miQuote
      where
         template = "<%s %s/%s>"
         venueName = symbolVal (Proxy :: Proxy venue)

instance Show AnyMarket where
   show (AnyMarket m) = show m

{-
class Json.FromJSON (OrderBook venue base quote) =>
         DataSource (venue :: Symbol) (base :: Symbol) (quote :: Symbol) where
   dataSrc :: DataSrc venue base quote

data DataSrc (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = DataSrc
   { dsUrl     :: S.BaseUrl
   , dsClientM :: SC.ClientM (OrderBook venue base quote)
   }

fetch :: forall venue base quote.
         DataSource venue base quote
      => HTTP.Manager
      -> IO (Either SC.ServantError (OrderBook venue base quote))
fetch man = SC.runClientM clientM env
   where env = SC.ClientEnv man (dsUrl (dataSrc :: DataSrc venue base quote))
         clientM = dsClientM (dataSrc :: DataSrc venue base quote)

 -}