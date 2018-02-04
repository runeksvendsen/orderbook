{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Lib.OrderBook.Types
( OrderBook(..)
, SomeBook
, mkSomeBook
, fromSomeBook
, Order(..)
, SomeOrder
, AnyBook(..)
, mkSomeOrder
, fromOrder
, midPrice
)
where

import MyPrelude
import qualified Money
--import Data.Vector hiding (null, concat)
import qualified Data.Vector  as Vec
import Text.Printf
import qualified Text.Show.Pretty   as P
import qualified Control.Category   as Cat


-- ETH/BTC . BTC/USD
--------------------
-- USD -> BTC -> ETH
-- ETH -> BTC -> USD

-- BuySide  ETH BTC . BuySide  BTC USD    (Buy  BTC for USD -> Buy  ETH for BTC)
-- SellSide BTC USD . SellSide ETH BTC    (Sell ETH for BTC -> Sell BTC for USD)

instance Cat.Category Order where
   id = Order (fromRational pseudoInf) Cat.id
      where pseudoInf = fromIntegral (maxBound :: Int64) % 1
   o1 . o2 = Order (fromRational minQty) (oPrice o1 Cat.. oPrice o2)
      where minQty = min (toRational $ oQuantity o1) (toRational $ oQuantity o2)

-- | The result of composing two orders, plus the remainder
composeRem :: Order b c
           -> Order a b
           -> (Order a c, Maybe (Either (Order b c) (Order a b)))
composeRem bc ab =
   fromDiff $ toRational (oQuantity bc) - toRational (oQuantity ab)
   where
   price = oPrice bc Cat.. oPrice ab
   fromDiff diff
      | diff == 0 = (Order (oQuantity ab) price, Nothing)
      | diff >  0 = (Order (oQuantity ab) price, Just . Left $
                     Order (fromRational diff) (oPrice bc))
      | diff <  0 = (Order (fromRational . toRational $ oQuantity bc) price, Just . Right $
                     Order (fromRational $ abs diff) (oPrice ab))


--newtype BuySide  base quote = BuySide (Vector (BuyOrder  base quote))
--newtype SellSide base quote = SellSide (Vector (SellOrder base quote))

data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: Vector (Order base quote)
   , obAsks  :: Vector (Order base quote)
   } deriving (Eq, Generic)

data SomeBook (venue :: Symbol) = SomeBook
   { sbBids  :: Vector SomeOrder
   , sbAsks  :: Vector SomeOrder
   }

mkSomeBook
   :: Vector SomeOrder  -- ^ Buy orders (bids)
   -> Vector SomeOrder  -- ^ Sell orders (asks)
   -> Either String (SomeBook venue)
mkSomeBook bids asks
   | null bids = Left "No bids"
   | null asks = Left "No asks"
   | otherwise = Right $ SomeBook bids asks


fromSomeBook
   :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
   => SomeBook venue
   -> OrderBook venue base quote
fromSomeBook SomeBook{..} =
   OrderBook (map fromSomeOrder sbBids)
             (map fromSomeOrder sbAsks)


{-
-- | Create a 'SomeBook' only if all 'SomeOrder's have the same
--    'Money.someDenseCurrency' and 'Money.someExchangeRateSrcCurrency',
--    and if both bids and asks are non-empty


withSomeBook
   :: forall venue r.
      KnownSymbol venue
   => SomeBook venue
   -> (forall base quote. (KnownSymbol base, KnownSymbol quote) => OrderBook venue base quote -> r)
   -> r
withSomeBook book f = let (base,quote) = soSrcDstCurrency . Vec.head . sbBids $ book in
   case someSymbolVal base of
      SomeSymbol baseP@(Proxy :: Proxy base) ->
         case someSymbolVal quote of
            SomeSymbol quoteP@(Proxy :: Proxy quote) ->
               f (fromMaybe (error . toS $ "BUG: fromSomeBook failure: " ++ show (show . Vec.head . sbBids $ book, base, quote)) $
                  fromSomeBook book :: OrderBook venue base quote)
-}

data Order (base :: Symbol) (quote :: Symbol) = Order
   { oQuantity :: Money.Dense base
   , oPrice    :: Money.ExchangeRate base quote
   } deriving (Eq, Generic)

data SomeOrder = SomeOrder
   { soQuantity :: Rational
   , soPrice    :: Rational
   } deriving (Eq, Generic, Show)

-- | Create a 'SomeOrder' only if price > 0 and neither 'notANumber' nor 'infinity'
mkSomeOrder :: Rational    -- ^ Quantity
            -> Rational    -- ^ Price
            -> Maybe SomeOrder
mkSomeOrder qty price =
   SomeOrder <$> fmap toRational (Money.dense qty)
             <*> fmap Money.fromExchangeRate (Money.exchangeRate price)

fromOrder
   :: Order base quote
   -> SomeOrder
fromOrder Order{..} =
   SomeOrder (toRational oQuantity)
             (Money.fromExchangeRate oPrice)

fromSomeOrder
   :: (KnownSymbol base, KnownSymbol quote)
   => SomeOrder
   -> Order base quote
fromSomeOrder so@SomeOrder{..} = -- We know SomeOrder contains valid Dense/ExchangeRate
   let throwBug = error . toS $ "SomeOrder: invalid qty/price: " ++ show so in
   Order (fromMaybe throwBug $ Money.dense soQuantity)
         (fromMaybe throwBug $ Money.exchangeRate soPrice)

{-
-- | Return src and dst currency (base and quote, respectively) for 'SomeOrder'
soSrcDstCurrency :: SomeOrder -> (String,String)
soSrcDstCurrency SomeOrder{..} =
   ( Money.someExchangeRateSrcCurrency soPrice
   , Money.someExchangeRateDstCurrency soPrice
   )

-}

instance Ord (Order base quote) where
   o1 <= o2 = oPrice o1 <= oPrice o2

data AnyBook venue = forall base quote.
   ( KnownSymbol venue
   , KnownSymbol base
   , KnownSymbol quote)
     => AnyBook (OrderBook venue base quote)


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
      else Just . unsafeConv $ ( rationalPrice bestBid
                               + rationalPrice bestAsk ) / 2

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

--instance (KnownSymbol base, KnownSymbol quote) => Show (BuyOrder base quote) where
--   show = showOrder "BUY " . buyOrder

--instance (KnownSymbol base, KnownSymbol quote) => Show (SellOrder base quote) where
--   show = showOrder "SELL" . sellOrder

--instance KnownSymbol venue => Show (SomeBook venue) where
--   show sb = withSomeBook sb show

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
         orders =    askIndent <> intercalate askIndent (showOrder "SELL" <$> sortDesc (Vec.toList obAsks))
                  <> bidIndent <> intercalate bidIndent (Vec.toList $ fmap (showOrder "BUY ") obBids)
      in printf template venue currPair midPriceStr orders

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Print (OrderBook venue base quote) where
   putStr ob = putStr (toS $ show ob :: Text)
   putStrLn l = putStr l >> putStr ("\n" :: Text)

