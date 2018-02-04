module Lib.Markets.Util
( fetchFromMarket
)
where

import MyPrelude
import Lib.Markets.Types
import Lib.Fetch
import Lib.OrderBook.Types
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP

fetchFromMarket
   :: forall venue.
      (KnownSymbol venue, MarketBook venue)
   => HTTP.Manager
   -> Market venue
   -> IO (Either SC.ServantError (AnyBook venue))
fetchFromMarket man market@Market{..} =
   fmap (bookFromMarket market) <$> srcFetch man (marketBook miApiSymbol)


bookFromMarket
   :: forall venue.
      KnownSymbol venue
   => Market venue
   -> SomeBook venue
   -> AnyBook venue
bookFromMarket market sb =
   case someSymbolVal (toS $ miBase market) of
      SomeSymbol (Proxy :: Proxy base) ->
         case someSymbolVal (toS $ miQuote market) of
               SomeSymbol (Proxy :: Proxy quote) ->
                  AnyBook (fromSomeBook sb :: OrderBook venue base quote)
