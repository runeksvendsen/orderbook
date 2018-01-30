{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Venues
( getMarkets
, venueNames
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
import Lib.Fetch
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Common.Req    as Req

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

venueNames :: [String]
venueNames = map (\(AnyVenue p) -> symbolVal p) allVenues

marketList'
   :: forall venue.
   ( KnownSymbol venue
   , DataSource (MarketList venue)
   )
   => HTTP.Manager
   -> Proxy venue
   -> IO (Either Req.ServantError (MarketList venue))
marketList' man _ = fetch man

marketList :: HTTP.Manager -> AnyVenue -> IO [AnyMarket]
marketList man (AnyVenue p) = do
   MarketList a <- failOnErr <$> marketList' man p
   return $ map AnyMarket a

getMarkets :: HTTP.Manager -> IO [AnyMarket]
getMarkets man = do
   markets <- mapM (marketList man) allVenues
   return (concat markets)





{-

forVenue
   :: forall venue userVenue.
      (KnownSymbol venue, KnownSymbol userVenue)
   => (Proxy venue -> IO ())
   -> Proxy userVenue
   -> IO ()
forVenue f userVenue =
   forM_ allVenues $ \(AnyVenue proxy) ->
       when (sameSym userVenue proxy) (f proxy)

 -}