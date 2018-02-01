{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Venues
(
--getMarkets
  venueNames
, AnyVenue(..)
--, stringVenue
--, marketList
--, toAnyVenueTH
)
where

import MyPrelude

import Venues.GDAXl2       as GDAXl2         ()
import Venues.GDAXl3       as GDAXl3         ()
import Venues.Bitstamp     as Bitstamp       ()
import Venues.Bitfinex     as Bitfinex       ()
import Venues.BitfinexV2   as BitfinexV2     ()
import Venues.Bittrex      as Bittrex        ()

import Lib.Markets
import Lib.OrderBook
import Lib.Fetch
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Common.Req    as Req
import qualified Servant.Client        as SC


fetchBook
   :: forall venue.
      (MarketBook venue, KnownSymbol venue)
   => HTTP.Manager
   -> Market venue
   -> IO (Either SC.ServantError (AnyBook venue))
fetchBook man market = do
   obE <- srcFetch man (marketBook market)
   case someSymbolVal (toS $ miBase market) of
      SomeSymbol (Proxy :: Proxy base) ->
         case someSymbolVal (toS $ miQuote market) of
               SomeSymbol (Proxy :: Proxy quote) ->
                  return $ AnyBook <$> (obE :: Either SC.ServantError (OrderBook venue base quote))


data AnyVenue
   = forall venue.
   ( KnownSymbol venue
   , DataSource (MarketList venue)
   )
   => AnyVenue (Proxy venue)

allVenues :: [AnyVenue]
allVenues =
   [ AnyVenue (Proxy :: Proxy "bitfinex")
   , AnyVenue (Proxy :: Proxy "bittrex")
--   ,  AnyVenue (Proxy :: Proxy "binance")
--   , AnyVenue (Proxy :: Proxy "bitstamp")
--   , AnyVenue (Proxy :: Proxy "gdax-l2")
--   , AnyVenue (Proxy :: Proxy "gdax-l3")
   ]

venueNames :: [(Text, AnyVenue)]
!venueNames = map (\v@(AnyVenue p) -> (toS $ symbolVal p, v)) allVenues
