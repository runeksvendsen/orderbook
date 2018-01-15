--{-# LANGUAGE DeriveAnyClass #-}
module Lib.OrderBook.Types where

import MyPrelude
import qualified Money
--import Data.Vector hiding (null, concat)
import qualified Data.Vector  as Vec
import Text.Printf
import qualified Text.Show.Pretty   as P


data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: Vector (BuyOrder  base quote)
   , obAsks  :: Vector (SellOrder base quote)
   } deriving (Eq, Generic)

data Order (base :: Symbol) (quote :: Symbol) = Order
   { oQuantity :: Money.Dense base
   , oPrice    :: Money.ExchangeRate base quote
   } deriving (Eq, Generic)

instance Ord (Order base quote) where
   o1 <= o2 = oPrice o1 <= oPrice o2

newtype BuyOrder  (base :: Symbol) (quote :: Symbol) = BuyOrder  { buyOrder :: Order base quote }
   deriving (Eq, Generic)
newtype SellOrder (base :: Symbol) (quote :: Symbol) = SellOrder { sellOrder :: Order base quote }
   deriving (Eq, Generic)

instance Ord (BuyOrder base quote) where
   o1 <= o2 = buyOrder o2 <= buyOrder o1     -- Buy orders with highest price first

instance Ord (SellOrder base quote) where
   o1 <= o2 = sellOrder o1 <= sellOrder o2   -- Sell orders with lowest price first

-- | Order book mid price (Nothing in case there are no bids and/or asks).
--   Will fail horribly if bestBidPrice+bestAskPrice equals zero
--   (which would comprise an invalid order book (containing a negative price)).
midPrice :: forall venue base quote.
            (KnownSymbol base, KnownSymbol quote)
         => OrderBook venue base quote
         -> Maybe (Money.ExchangeRate base quote)
midPrice OrderBook{..} =
   let rationalPrice = Money.fromExchangeRate . oPrice
       ~bestBid = obBids Vec.! 0
       ~bestAsk = obAsks Vec.! 0
       unsafeConv r = fromMaybe (error . toS $ "Bad midPrice: " <> show (r,bestBid,bestAsk))
                                (Money.exchangeRate r)
   in
   if null obBids || null obAsks
      then Nothing
      else Just . unsafeConv $ ( rationalPrice (buyOrder bestBid)
                               + rationalPrice (sellOrder bestAsk) ) / 2

showOrder :: forall base quote.
             (KnownSymbol base, KnownSymbol quote)
          => String
          -> Order base quote
          -> String
showOrder name Order{..} =
   let
      template = "[%s: %.4f %s @ %.4f %s/%s]"
      baseS = symbolVal (Proxy :: Proxy base)
      quoteS = symbolVal (Proxy :: Proxy quote)
      doublePrice :: Double
      doublePrice = realToFrac . Money.fromExchangeRate $ oPrice
      doubleQty :: Double
      doubleQty = realToFrac oQuantity
   in printf template name doubleQty baseS doublePrice quoteS baseS

instance (KnownSymbol base, KnownSymbol quote) => Show (Order base quote) where
   show = showOrder "Order"

--instance (KnownSymbol base, KnownSymbol quote) => Show [Order base quote] where
--   show = concat . fmap (showOrder "Order")

instance (KnownSymbol base, KnownSymbol quote) => Show (BuyOrder base quote) where
   show = showOrder "BUY " . buyOrder

instance (KnownSymbol base, KnownSymbol quote) => Show (SellOrder base quote) where
   show = showOrder "SELL" . sellOrder

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Show (OrderBook venue base quote) where
   show ob@OrderBook{..} =
      let
         template = "<Order book: %s %s, mid price: %s, orders: %s>"
         venue = symbolVal (Proxy :: Proxy venue)
         currPair = symbolVal (Proxy :: Proxy base) <> "/" <> symbolVal (Proxy :: Proxy quote)
         midPriceF :: Maybe Double
         midPriceF = realToFrac . Money.fromExchangeRate <$> midPrice ob
         sortDesc = sortBy (flip compare)
         askIndent = ("\n\t" <> replicate 40 ' ')
         bidIndent = "\n\t"
         midPriceStr :: String
         midPriceStr = case midPriceF of
                        Nothing -> "<no bids/asks>"
                        Just price -> printf "%.4f" price
         orders =    askIndent <> intercalate askIndent (show <$> sortDesc (Vec.toList obAsks))
                  <> bidIndent <> intercalate bidIndent (Vec.toList $ fmap show obBids)
      in printf template venue currPair midPriceStr orders

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Print (OrderBook venue base quote) where
   putStr ob = putStr (toS $ show ob :: Text)
   putStrLn l = putStr l >> putStr ("\n" :: Text)

