module Venues.BitfinexV2
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
import qualified Data.Vector  as Vec
import qualified Money
import Control.Monad.Fail
import qualified Data.Scientific as Sci

-- | https://docs.bitfinex.com/v2/reference#rest-public-books


instance Json.FromJSON (OrderBook "BitfinexV2" base quote) where
   parseJSON val = Json.parseJSON val >>= parseBook

-- | https://docs.bitfinex.com/v2/reference#rest-public-books
type Book = Vector BitfinexOrder

-- | https://docs.bitfinex.com/v2/reference#rest-public-books
type BitfinexOrder = (Sci.Scientific   -- PRICE
                     ,Word             -- COUNT
                     ,Sci.Scientific)  -- AMOUNT

parseOrder :: BitfinexOrder -> Json.Parser (Order base quote)
parseOrder (price,_,qty) = Order <$> mkQty qty <*> mkPrice price
   where
      mkQty sci = failWith ("Bad quantity: " <> show sci) $ Money.dense (toRational sci)
      mkPrice sci = failWith ("Bad price: " <> show sci) $ Money.exchangeRate (toRational sci)
      failWith err = maybe (fail err) return

parseBook :: Book -> Json.Parser (OrderBook venue base quote)
parseBook book = do
   orders <- traverse parseOrder book
   return $ OrderBook (buyOrders orders) (sellOrders orders)
   where
      buyOrders = fmap BuyOrder . Vec.filter ((> 0) . oQuantity)
      sellOrders = fmap (SellOrder . fixSellQty) . Vec.filter ((< 0) . oQuantity)
      fixSellQty order@Order{..} = order { oQuantity = abs oQuantity }

bitfinex :: S.BaseUrl
bitfinex = S.BaseUrl S.Https "api.bitfinex.com" 443 ""

type Api base quote
   = "v2"
   :> "book"
   :> Capture "symbol" Text
   :> "P0"
   :> QueryParam "len" Word
   :> Get '[JSON] (OrderBook "BitfinexV2" base quote)

instance DataSource (OrderBook "BitfinexV2" "BTC" "USD") where
   dataSrc = DataSrc bitfinex (clientM "tBTCUSD" (Just 250))
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))


