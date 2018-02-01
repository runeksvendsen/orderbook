module Lib.OrderBook.New
(


)
where

import MyPrelude
import qualified Money
--import Data.Vector hiding (null, concat)
import qualified Data.Vector  as Vec
import Text.Printf
import qualified Text.Show.Pretty   as P


-- "BTCUSD" Sell: "BTC" -> "USD"
--          Buy : "USD" -> "BTC"


data SellOrder (src :: Symbol) (dst :: Symbol) = SellOrder
   { oQuantity :: Money.Dense src
   , oPrice    :: Money.ExchangeRate src dst
   } deriving (Eq, Generic)

data BuyOrder (src :: Symbol) (dst :: Symbol) = BuyOrder
   { oQuantity :: Money.Dense src
   , oPrice    :: Money.ExchangeRate dst src
   } deriving (Eq, Generic)




--instance Ord (Order src dst) where
--   o1 <= o2 = oPrice o1 <= oPrice o2



-- | Order book containing buy and sell orders
--data OrderBook (venue :: Symbol) (src :: Symbol) (dst :: Symbol) = OrderBook
--   { obBids  :: Vector (BuyOrder  src dst) -- ^ Buyers have 'dst' and want 'src'
--   , obAsks  :: Vector (SellOrder src dst) -- ^ Sellers have 'src' and want 'dst'
--   } deriving (Eq, Generic)


{-
-- | Order book mid price (Nothing in case there are no bids and/or asks).
--   Will fail horribly if bestBidPrice+bestAskPrice equals zero
--   (which would comprise an invalid order book (containing a negative price)).
midPrice :: forall venue src dst.
            (KnownSymbol src, KnownSymbol dst)
         => OrderBook venue src dst
         -> Maybe (Money.ExchangeRate src dst)
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

--instance (KnownSymbol src, KnownSymbol dst) => Show [Order src dst] where
--   show = concat . fmap (showOrder "Order")

instance (KnownSymbol src, KnownSymbol dst) => Show (BuyOrder src dst) where
   show = showOrder "BUY " . buyOrder

instance (KnownSymbol src, KnownSymbol dst) => Show (SellOrder src dst) where
   show = showOrder "SELL" . sellOrder

instance (KnownSymbol venue, KnownSymbol src, KnownSymbol dst) =>
            Show (OrderBook venue src dst) where
   show ob@OrderBook{..} =
      let
         template = "<Order book: %s %s, mid price: %s, orders: %s>"
         venue = symbolVal (Proxy :: Proxy venue)
         currPair = symbolVal (Proxy :: Proxy src) <> "/" <> symbolVal (Proxy :: Proxy dst)
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

instance (KnownSymbol venue, KnownSymbol src, KnownSymbol dst) =>
            Print (OrderBook venue src dst) where
   putStr ob = putStr (toS $ show ob :: Text)
   putStrLn l = putStr l >> putStr ("\n" :: Text)
-}

{-
showOrder :: forall src dst.
             (KnownSymbol src, KnownSymbol dst)
          => String
          -> Order src dst
          -> String
showOrder name Order{..} =
   let
      template = "[%s: %.4f %s @ %.4f %s/%s]"
      baseS = symbolVal (Proxy :: Proxy src)
      quoteS = symbolVal (Proxy :: Proxy dst)
      doublePrice :: Double
      doublePrice = realToFrac . Money.fromExchangeRate $ oPrice
      doubleQty :: Double
      doubleQty = realToFrac oQuantity
   in printf template name doubleQty baseS doublePrice quoteS baseS

instance (KnownSymbol src, KnownSymbol dst) => Show (Order src dst) where
   show = showOrder "Order"
-}