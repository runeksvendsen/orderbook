module Api.Handler where

import MyPrelude
import Api.Util
import Lib.Markets.Types
import Lib.Fetch
import Venues -- (venueNames, stringVenue)
import Control.Monad.Except            (throwError)
import qualified Network.HTTP.Client   as HTTP
import qualified Servant.Server        as SS


listVenues :: SS.Handler [Text]
listVenues = return $ map fst venueNames

listMarkets :: HTTP.Manager -> Text -> SS.Handler [AnyMarket]
listMarkets man venueName =
   case lookup venueName venueNames of
      Nothing    -> throwError SS.err404
      Just venue -> liftIO (marketList man venue)
                       >>= either (throwError . toServantErr) return


slippageSell :: HTTP.Manager -> Text -> Text -> SS.Handler [AnyMarket]
slippageSell man venue market = undefined

slippageBuy :: HTTP.Manager -> Text -> Text -> SS.Handler [AnyMarket]
slippageBuy man venue market = undefined
