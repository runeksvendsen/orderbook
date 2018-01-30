{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Venues.Bitfinex
()
where

import MyPrelude     hiding (asks)
import Lib.OrderBook
import Lib.Markets
import Venues.Common.StringArrayOrder  (parseOrderStr)
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import Servant.API
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
--import qualified Data.Aeson.Parser as Json
import           Data.Vector  (Vector)
import Control.Monad.Fail
import qualified Data.Text as T


instance Json.FromJSON (OrderBook "bitfinex" base quote) where
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
   :> Get '[JSON] (OrderBook "bitfinex" base quote)

-- TODO: symbolVal?
instance DataSource (OrderBook "bitfinex" "BTC" "USD") where
   dataSrc = DataSrc bitfinexUrl (clientM "btcusd" (Just 1000) (Just 1000))
      where
         clientM = SC.client (Proxy :: Proxy (Api "BTC" "USD"))

instance DataSource (MarketList "bitfinex") where
   dataSrc = DataSrc bitfinexUrl clientM
      where
         clientM = SC.client (Proxy :: Proxy ApiMarkets)


-- | https://api.bitfinex.com/v1/symbols
type ApiMarkets
   = "v1"
   :> "symbols"
   :> Get '[JSON] (MarketList "bitfinex")

newtype TxtLst = TxtLst [Text] deriving Json.FromJSON

instance Json.FromJSON (MarketList "bitfinex") where
   parseJSON val = MarketList <$> Json.parseJSON val

instance Json.FromJSON (Market "bitfinex") where
   parseJSON = Json.withText "Bitfinex market" $ \currPair ->
         if T.length currPair /= 6
            then fail $ "Invalid symbol: " ++ toS currPair
            else return Market
                  { miBase       = T.toUpper $ T.take 3 currPair
                  , miQuote      = T.toUpper $ T.takeEnd 3 currPair
                  , miApiSymbol  = currPair
                  }

--instance MarketInfo "bittrex" base quote where
--   marketBook Market{..} = mkBookSrc miApiSymbol

