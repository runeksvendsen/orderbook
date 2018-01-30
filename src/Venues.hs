{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Venues
( getMarkets
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
   [
     AnyVenue (Proxy :: Proxy "Bitfinex")
--   ,  AnyVenue (Proxy :: Proxy "Binance")

--   , AnyVenue (Proxy :: Proxy "Bitstamp")
   , AnyVenue (Proxy :: Proxy "Bittrex")
--   , AnyVenue (Proxy :: Proxy "GDAXl2")
--   , AnyVenue (Proxy :: Proxy "GDAXl3")
   ]

marketNames
   :: [AnyVenue]
   -> [String]
marketNames = map (\(AnyVenue p) -> symbolVal p)

marketList' :: forall venue.
              ( KnownSymbol venue
              , DataSource (MarketList venue)
              )
           => HTTP.Manager
           -> Proxy venue
           -> IO (Either Req.ServantError (MarketList venue))
marketList' man _ =
   fetch man

marketList :: HTTP.Manager -> AnyVenue -> IO [AnyMarket]
marketList man (AnyVenue p) = do
   MarketList a <- failOnErr . fmapL show <$> marketList' man p
   return $ map AnyMarket a

getMarkets :: HTTP.Manager -> IO [AnyMarket]
getMarkets man = do
--   MarketList bitfinex :: MarketList "Bitfinex" <- failOnErr . fmapL show <$> fetch man
--   MarketList bittrex  :: MarketList "Bittrex"  <- failOnErr . fmapL show <$> fetch man
   markets <- mapM (marketList man) allVenues
   return (concat markets)


failOnErr :: forall a venue. KnownSymbol venue => Either String (a venue) -> a venue
failOnErr = either (\str -> error . toS $ symbolVal (Proxy :: Proxy venue) <> ": " <> str) id

