module Venues.GDAXl2

()
where

import MyPrelude
import Lib.OrderBook
import Venues.Common.StringArrayOrder  (parseOrderStr)
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json

instance Json.FromJSON (OrderBook "gdax-l2" base quote) where
   parseJSON val =
      let fromBook Book{..} = OrderBook
            <$> traverse parseOrder bids
            <*> traverse parseOrder asks
      in Json.parseJSON val >>= fromBook

data Book = Book
   { sequence :: Word            -- https://docs.gdax.com/#sequence-numbers
   , bids :: Vector GDAXl2Order
   , asks :: Vector GDAXl2Order
   } deriving (Eq, Show, Generic)

instance Json.FromJSON Book

type GDAXl2Order = (String,String,Word)   -- Price, Quantity, Order_Count

parseOrder :: GDAXl2Order -> Json.Parser (Order base quote)
parseOrder (price,qty,_) = parseOrderStr price qty

gdax :: SC.BaseUrl
gdax = S.BaseUrl S.Https "api.gdax.com" 443 ""

type Api base quote
   = "products"
   :> Capture "symbol" Text
   :> "book"
   :> QueryParam "level" Word
   :> Header "User-Agent" Text
   :> Get '[JSON] (OrderBook "gdax-l2" base quote)

instance DataSource (OrderBook "gdax-l2" "BTC" "USD") where
   dataSrc = DataSrc gdax (clientM "BTC-USD" (Just 2) (Just userAgent))
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))

instance DataSource (OrderBook "gdax-l2" "BTC" "EUR") where
   dataSrc = DataSrc gdax (clientM "BTC-EUR" (Just 2) (Just userAgent))
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "EUR"))

-- TODO
userAgent :: Text
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36"
