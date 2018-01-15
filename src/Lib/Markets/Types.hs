module Lib.Markets.Types where


import MyPrelude
import Lib.OrderBook.Types
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


--class MarketInfo (venue :: Symbol) where
--   marketInfo ::

data Market (venue :: Symbol) = Market
   { miBase       :: Text
   , miQuote      :: Text
   , miApiSymbol  :: Text
   }

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