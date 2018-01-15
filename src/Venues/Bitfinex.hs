module Venues.Bitfinex
()
where

import MyPrelude     hiding (asks)
import Lib.OrderBook
import Venues.Common.StringArrayOrder  (parseOrderStr)
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail


instance Json.FromJSON (OrderBook "Bitfinex" base quote) where
   parseJSON val =
      let fromBook Book{..} = OrderBook
            <$> traverse (fmap BuyOrder . parseOrder)  bids
            <*> traverse (fmap SellOrder . parseOrder) asks
      in Json.parseJSON val >>= fromBook

data Book = Book
   { bids :: Vector BitfinexOrder
   , asks :: Vector BitfinexOrder
   } deriving (Eq, Show, Generic)

data BitfinexOrder = BitfinexOrder
   { price     :: String
   , amount    :: String
   , timestamp :: String
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book
instance Json.FromJSON BitfinexOrder

parseOrder :: BitfinexOrder -> Json.Parser (Order base quote)
parseOrder BitfinexOrder{..} = parseOrderStr price amount

bitfinexUrl :: S.BaseUrl
bitfinexUrl = S.BaseUrl S.Https "api.bitfinex.com" 443 ""

type Api base quote
   = "v1"
   :> "book"
   :> Capture "symbol" Text
   :> QueryParam "limit_bids" Word
   :> QueryParam "limit_asks" Word
   :> Get '[JSON] (OrderBook "Bitfinex" base quote)

instance DataSource (OrderBook "Bitfinex" "BTC" "USD") where
   dataSrc = DataSrc bitfinexUrl (clientM "btcusd" (Just 1000) (Just 1000))
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))
