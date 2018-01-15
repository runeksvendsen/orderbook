module Venues.Bitstamp
()
where

import MyPrelude
import Lib.OrderBook
import Venues.Common.StringArrayOrder  (parseOrderStr)
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail


instance Json.FromJSON (OrderBook "Bitstamp" base quote) where
   parseJSON val =
      let fromBook Book{..} = OrderBook
            <$> traverse (fmap BuyOrder . parseOrder)  bids
            <*> traverse (fmap SellOrder . parseOrder) asks
      in Json.parseJSON val >>= fromBook

data Book = Book
   { timestamp :: Text
   , bids :: Vector BitstampOrder
   , asks :: Vector BitstampOrder
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book

type BitstampOrder = (String,String)   -- Price, Quantity

parseOrder :: BitstampOrder -> Json.Parser (Order base quote)
parseOrder (price,qty) = parseOrderStr price qty


bitstamp = S.BaseUrl S.Https "www.bitstamp.net" 443 ""

type Api base quote
   = "api"
   :> "v2"
   :> "order_book"
   :> Capture "symbol" Text
   :> Get '[JSON] (OrderBook "Bitstamp" base quote)

instance DataSource (OrderBook "Bitstamp" "BTC" "USD") where
   dataSrc = DataSrc bitstamp (clientM "btcusd")
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))

