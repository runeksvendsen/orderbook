module Api.Handler where

import MyPrelude
import Api.Util
import Lib.Markets
import Lib.Fetch
import Lib.OrderBook
import Venues
--import Control.Monad.Except            (throwError)
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Server        as SS
import Control.Monad.Error.Class       (throwError)


listVenues :: SS.Handler [Text]
listVenues = return venueNames

listMarkets
   :: HTTP.Manager
   -> Text
   -> SS.Handler [AnyMarket]
listMarkets man venueName =
   withVenue venueName $ \venue ->
      throwErr =<< liftIO (marketList man venue)

slipSell :: HTTP.Manager
         -> Text
         -> Text
         -> Double
         -> SS.Handler SlippageInfo
slipSell man venueName market slip =
   withMarket venueName market $ \(AnyMarket market) -> do
      AnyBook ob <- throwErr =<< liftIO (fetchFromMarket man market)
      return $ fromMatchRes (slippageSell ob (realToFrac slip))

slipBuy :: HTTP.Manager
        -> Text
        -> Text
        -> Double
        -> SS.Handler SlippageInfo
slipBuy man venueName market slip =
   withMarket venueName market $ \(AnyMarket market) -> do
      AnyBook ob <- throwErr =<< liftIO (fetchFromMarket man market)
      return $ fromMatchRes (slippageBuy ob (realToFrac slip))


-- Helpers
withVenue :: Text -> (AnyVenue -> SS.Handler r) -> SS.Handler r
withVenue venueName f =
   case venueLookup venueName of
      Nothing    -> throwError SS.err404
      Just venue -> f venue

withMarket :: Text -> Text -> (AnyMarket -> SS.Handler r) -> SS.Handler r
withMarket venueName marketName f =
   withVenue venueName $ \venue ->
      case fromString venue marketName of
         Just anyMarket -> f anyMarket
         Nothing        -> throwError
            SS.err400 { SS.errReasonPhrase = "Invalid market: " <> toS marketName
                      , SS.errBody = "Failed to parse market from string: " <> toS marketName
                      }


