module Api.Util where

import MyPrelude
import Venues
import Lib.OrderBook
import Lib.Markets
import Lib.Fetch
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Common.Req    as Req
import qualified Servant.Server        as SS
import qualified Network.HTTP.Types.Status as HTTP


-- Util

-- | Convert 'Servant.Client' response status error to 'Servant.Server' throwable error
toServantErr :: Req.ServantError -> SS.ServantErr
toServantErr Req.FailureResponse{..} =
   SS.ServantErr
      { errHTTPCode = HTTP.statusCode responseStatus
      , errReasonPhrase = "API failure response"
      , errBody = responseBody
      , errHeaders = []
      }
toServantErr Req.DecodeFailure{..} = let err = "Decode error: " ++ decodeError in
   SS.err500 { SS.errBody = toS err, SS.errReasonPhrase = err }
toServantErr ex = let err = show ex in
   SS.err500 { SS.errBody = toS err, SS.errReasonPhrase = err }

{-
orderBook'
   :: forall venue.
   ( KnownSymbol venue
   , DataSource (MarketList venue)
   )
   => HTTP.Manager
   -> Proxy venue
   -> IO (Either Req.ServantError (OrderBook venue base quote))
orderBook' man _ = fetch man
-}

marketList'
   :: forall venue.
   ( KnownSymbol venue
   , DataSource (MarketList venue)
   )
   => HTTP.Manager
   -> Proxy venue
   -> IO (Either Req.ServantError (MarketList venue))
marketList' man _ = fetch man

marketList :: HTTP.Manager -> AnyVenue -> IO (Either Req.ServantError [AnyMarket])
marketList man (AnyVenue p) = do
   resE <- marketList' man p
   let getMarket (MarketList a) = a
   return $ map AnyMarket . getMarket <$> resE

--getMarkets :: HTTP.Manager -> IO [AnyMarket]
--getMarkets man = do
--   markets <- mapM (marketList man) allVenues
--   return (concat markets)
