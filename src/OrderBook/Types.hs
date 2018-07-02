{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module OrderBook.Types
( OrderBook(..)
, BuySide(..), buySide, bestBid
, SellSide(..), sellSide, bestAsk
, SellOrders(..), BuyOrders(..)
, OrderbookSide(..)
, SomeBook
, mkSomeBook
, fromSomeBook
, Order(..)
, SomeOrder
, AnyBook(..)
, mkSomeOrder
, fromOrder
, midPrice
, showOrders
  -- * Test
, composeRem
)
where

import MyPrelude
import qualified Money
import qualified Data.Vector  as Vec
import Text.Printf
import qualified Control.Category   as Cat


-- | Buyers want to convert "quote" to "base"
newtype BuySide (venue :: Symbol) (base :: Symbol) (quote :: Symbol)
   = BuySide { buySide :: Vector (Order base quote) }
      deriving (Eq, Generic)

-- | Sellers want to convert "base" to "quote"
newtype SellSide (venue :: Symbol) (base :: Symbol) (quote :: Symbol)
   = SellSide { sellSide :: Vector (Order base quote) }
      deriving (Eq, Generic)

bestBid :: OrderBook venue base quote -> Maybe (Order base quote)
bestBid = (Vec.!? 0) . buySide . obBids

bestAsk :: OrderBook venue base quote -> Maybe (Order base quote)
bestAsk = (Vec.!? 0) . sellSide . obAsks

instance Cat.Category (BuySide venue) where
   id = BuySide (Vec.fromList $ repeat largeQtyIdOrder)
   (BuySide b1) . (BuySide b2) = BuySide $ Vec.fromList orders
      where orders = composeLst (Vec.toList b1) (Vec.toList b2)

instance Cat.Category (SellSide venue) where
   id = SellSide (Vec.fromList $ repeat largeQtyIdOrder)
   (SellSide b1) . (SellSide b2) = SellSide $ Vec.fromList orders
      where orders = composeLst (Vec.toList b1) (Vec.toList b2)

-- | Compose two lists of orders
composeLst
    :: [Order b c]
    -> [Order a b]
    -> [Order a c]
composeLst bcL =
    reverse . composeLstR [] bcL
  where
    composeLstR accum [] _ = accum
    composeLstR accum _ [] = accum
    composeLstR accum (bc:bcs) (ab:abs) =
        let (ac, rem) = composeRem bc ab
            toPair Nothing              = (bcs, abs)
            toPair (Just (Left  bcRem)) = (bcRem : bcs, abs)
            toPair (Just (Right abRem)) = (bcs, abRem : abs)
        in composeLstR (ac : accum) `uncurry` toPair rem

-- | The result of composing two orders, plus the remainder (if any)
composeRem :: Order b c
           -> Order a b
           -> (Order a c, Maybe (Either (Order b c) (Order a b)))
composeRem bc ab =
   fromDiff $ qtyBC `compare` qtyAB
   where
   price = oPrice bc Cat.. oPrice ab
   qtyAB = toRational (oQuantity ab)
   qtyBC = toRational (oQuantity bc)
   -- No remainder ("Order a b" quantity is equal to that of "Order b c")
   fromDiff EQ = (Order (oQuantity ab) price, Nothing)
   -- Remainder of type "Order b c" ("Order b c" quantity is greater than that of "Order a b")
   fromDiff GT = (Order (oQuantity ab) price, Just . Left $
                  Order (Money.dense' $ qtyBC-qtyAB) (oPrice bc))
   -- Remainder of type "Order a b" ("Order b c" quantity is less than that of "Order a b")
   fromDiff LT = (Order (Money.dense' . toRational $ oQuantity bc) price, Just . Right $
                  Order (Money.dense' $ qtyAB-qtyBC) (oPrice ab))

largeQtyIdOrder :: Order base base
largeQtyIdOrder =
    Order (Money.dense' pseudoInf) Cat.id
  where
    pseudoInf = fromIntegral (maxBound :: Int64) % 1

class OrderbookSide a (base :: Symbol) (quote :: Symbol) where
    isEmpty  :: a base quote -> Bool                -- ^ No orders?
    totalQty :: a base quote -> Money.Dense base    -- ^ Total order quantity

instance OrderbookSide (BuySide venue) base quote where   
    isEmpty = null . buySide
    totalQty = sum . map oQuantity . Vec.toList . buySide

instance OrderbookSide (SellSide venue) base quote where   
    isEmpty = null . sellSide
    totalQty = sum . map oQuantity . Vec.toList . sellSide

data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: BuySide venue base quote
   , obAsks  :: SellSide venue base quote
   } deriving (Eq, Generic)

instance NFData (BuySide venue base quote)
instance NFData (SellSide venue base quote)
instance NFData (OrderBook venue base quote)

instance Cat.Category (OrderBook venue) where
   id = OrderBook Cat.id Cat.id
   (OrderBook b1 s1) . (OrderBook b2 s2) =
        OrderBook (b1 Cat.. b2) (s1 Cat.. s2)

class SellOrders a (base :: Symbol) (quote :: Symbol) where
    sellOrders :: a base quote -> Vector (Order base quote)

class BuyOrders a (base :: Symbol) (quote :: Symbol) where
    buyOrders :: a base quote -> Vector (Order base quote)

instance SellOrders (SellSide venue) base quote where sellOrders = sellSide
instance SellOrders (OrderBook venue) base quote where sellOrders = sellSide . obAsks

instance BuyOrders (BuySide venue) base quote where buyOrders = buySide
instance BuyOrders (OrderBook venue) base quote where buyOrders = buySide . obBids


data SomeBook (venue :: Symbol) = SomeBook
   { sbBids  :: Vector SomeOrder
   , sbAsks  :: Vector SomeOrder
   }

mkSomeBook
   :: Vector SomeOrder  -- ^ Buy orders (bids)
   -> Vector SomeOrder  -- ^ Sell orders (asks)
   -> Either String (SomeBook venue)
mkSomeBook bids = Right . SomeBook bids

fromSomeBook
   :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
   => SomeBook venue
   -> OrderBook venue base quote
fromSomeBook SomeBook{..} =
   OrderBook (BuySide $ map fromSomeOrder sbBids)
             (SellSide $ map fromSomeOrder sbAsks)


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
             <*> fmap Money.exchangeRateToRational (Money.exchangeRate price)

fromOrder
   :: Order base quote
   -> SomeOrder
fromOrder Order{..} =
   SomeOrder (toRational oQuantity)
             (Money.exchangeRateToRational oPrice)

fromSomeOrder
   :: (KnownSymbol base, KnownSymbol quote)
   => SomeOrder
   -> Order base quote
fromSomeOrder so@SomeOrder{..} = -- We know SomeOrder contains valid Dense/ExchangeRate
   let throwBug = error $ "SomeOrder: invalid qty/price: " ++ show so in
   Order (fromMaybe throwBug $ Money.dense soQuantity)
         (fromMaybe throwBug $ Money.exchangeRate soPrice)

instance Ord (Order base quote) where
   o1 <= o2 = oPrice o1 <= oPrice o2

instance NFData (Order base quote)

data AnyBook venue = forall base quote.
   ( KnownSymbol venue
   , KnownSymbol base
   , KnownSymbol quote )
     => AnyBook (OrderBook venue base quote)

-- | Order book mid price (Nothing in case there are no bids and/or asks).
--   Will fail horribly if bestBidPrice+bestAskPrice equals zero
--   (which would comprise an invalid order book (containing a negative price)).
midPrice :: forall venue base quote.
            (KnownSymbol base, KnownSymbol quote)
         => OrderBook venue base quote
         -> Maybe (Money.ExchangeRate base quote)
midPrice ob@OrderBook{..} =
   let rationalPrice = Money.exchangeRateToRational . oPrice
       unsafeConv r = fromMaybe (error $ "Bad midPrice: " <> show (r,bestBid ob,bestAsk ob))
                                (Money.exchangeRate r)
   in do
      bb <- bestBid ob
      ba <- bestAsk ob
      return . unsafeConv $ ( rationalPrice bb
                            + rationalPrice ba ) / 2

showOrder :: forall base quote.
             (KnownSymbol base, KnownSymbol quote)
          => String
          -> Maybe (Money.Dense base, Money.Dense quote)
          -> Order base quote
          -> String
showOrder name cumVolM Order{..} =
   let
      template =  "[%s: %.4f%s @ %.4f %s/%s%s"
      baseS = symbolVal (Proxy :: Proxy base)
      quoteS = symbolVal (Proxy :: Proxy quote)
      doublePrice :: Double
      doublePrice = realToFrac . Money.exchangeRateToRational $ oPrice
      doubleQty :: Double
      doubleQty = realToFrac oQuantity
      mkCumVol (baseV, quoteV) = printf " (cum. %.4f%s/%.4f%s)]" 
                                   (realToFrac baseV :: Double) baseS
                                   (realToFrac quoteV :: Double) quoteS
      maybeVol :: String
      maybeVol = maybe "]" mkCumVol cumVolM
   in printf template name doubleQty baseS doublePrice quoteS baseS maybeVol

instance (KnownSymbol base, KnownSymbol quote) => Show (Order base quote) where
   show = showOrder "Order" Nothing

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Show (OrderBook venue base quote) where
   show ob@OrderBook{..} =
      let
         template = "\n<Order book: %s %s, mid price: %s, orders: %s>"
         venue = symbolVal (Proxy :: Proxy venue)
         currPair = symbolVal (Proxy :: Proxy base) <> "/" <> symbolVal (Proxy :: Proxy quote)
         midPriceF :: Maybe Double
         midPriceF = realToFrac . Money.exchangeRateToRational <$> midPrice ob
         askIndent = ("\n\t" <> replicate 40 ' ')
         bidIndent = "\n\t"
         midPriceStr :: String
         midPriceStr = case midPriceF of
                        Nothing -> "<no bids/asks>"
                        Just price -> printf "%.4f" price
         orders =    askIndent <> intercalate askIndent (lines $ show obAsks)
                  <> bidIndent <> intercalate bidIndent (lines $ show obBids)
      in printf template venue currPair midPriceStr orders

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Show (SellSide venue base quote) where
   show SellSide{..} = 
        unlines . showOrders "SELL" . Vec.toList $ sellSide

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Show (BuySide venue base quote) where
   show BuySide{..} = 
        unlines . reverse . showOrders "BUY" . Vec.toList $ buySide

showOrders
    :: (KnownSymbol base, KnownSymbol quote) 
    => String                       -- ^ Prefix (e.g. "Sell", "Buy")
    -> [Order base quote]    -- ^ Orders
    -> [String]                     -- ^ One string per line of output
showOrders prefix orders = fst $ 
    foldl' (countVol prefix) 
           ([], (Money.dense' 0, Money.dense' 0))  
           orders

-- Helper function for "instance Show BuySide/SellSide"
countVol :: (KnownSymbol base, KnownSymbol quote)
         => String
         -> ([String], (Money.Dense base, Money.Dense quote))
         -> Order base quote 
         -> ([String], (Money.Dense base, Money.Dense quote))
countVol prefix (strL, (cumVolB, cumVolQ)) o@Order{..} = 
    let newVolB = cumVolB+oQuantity 
        newVolQ = cumVolQ+Money.exchange oPrice oQuantity
    in ( showOrder prefix (Just (newVolB,newVolQ)) o : strL
       , (newVolB, newVolQ)
       )

instance Show (AnyBook venue) where
   show (AnyBook ob) = show ob

instance NFData (AnyBook venue) where
   rnf (AnyBook ob) = rnf ob

instance (KnownSymbol venue, KnownSymbol base, KnownSymbol quote) =>
            Print (OrderBook venue base quote) where
   putStr ob = putStr (toS $ show ob :: Text)
   putStrLn l = putStr l >> putStr ("\n" :: Text)

