{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module OrderBook.Types
( OrderBook(..)
, BuySide(BuySide), buySide, bestBid
, SellSide(SellSide), sellSide, bestAsk
, SellOrders(..), BuyOrders(..)
, OrderbookSide(..)
, SomeBook
, mkSomeBook
, fromSomeBook
, toSomeBook
, someBookOrders
, Order(..)
, SomeOrder
, AnyBook(..)
, mkSomeOrder
, fromOrder
, midPrice
, showOrders
, showBookUgly
, Invertible(..)
  -- * Test
, composeRem
, unsafeCastOrderbook
)
where

import MyPrelude
import qualified OrderBook.Util     as Util
import qualified Money
import qualified Data.Vector  as Vec
import qualified Control.Category   as Cat
import qualified Data.Aeson         as Json
import           Data.Aeson         ((.=), (.:))


-- | Buyers want to convert "quote" to "base"
newtype BuySide (base :: Symbol) (quote :: Symbol)
   = BuySide { buySide :: Vector (Order base quote) }
      deriving (Eq, Generic)

-- | Sellers want to convert "base" to "quote"
newtype SellSide (base :: Symbol) (quote :: Symbol)
   = SellSide { sellSide :: Vector (Order base quote) }
      deriving (Eq, Generic)

bestBid :: OrderBook venue base quote -> Maybe (Order base quote)
bestBid = (Vec.!? 0) . buySide . obBids

bestAsk :: OrderBook venue base quote -> Maybe (Order base quote)
bestAsk = (Vec.!? 0) . sellSide . obAsks

instance Cat.Category BuySide where
   id = BuySide (Vec.fromList $ repeat largeQtyIdOrder)
   (BuySide b1) . (BuySide b2) = BuySide $ Vec.fromList orders
      where orders = composeLst (merge $ Vec.toList b1) (merge $ Vec.toList b2)

instance Cat.Category SellSide where
   id = SellSide (Vec.fromList $ repeat largeQtyIdOrder)
   (SellSide b1) . (SellSide b2) = SellSide $ Vec.fromList orders
      where orders = composeLst (merge $ Vec.toList b1) (merge $ Vec.toList b2)

-- | merge adjacent orders with same price (ignoring venue)
merge
    :: [Order base quote]
    -- ^ List of orders
    -> [Order base quote]
merge =
    Util.combine tryMergeOrders
  where
    mergeOrders order1 order2 = order1 { oQuantity = oQuantity order1 + oQuantity order2 }
    tryMergeOrders order1 order2 =
        if oPrice order1 == oPrice order2
            then Just (mergeOrders order1 order2)
            else Nothing


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
composeRem :: forall a b c.
              Order b c
           -> Order a b
           -> (Order a c, Maybe (Either (Order b c) (Order a b)))
composeRem bc ab =
   fromDiff $ oQuantity bc `compare` oABQtyB
   where
   -- No remainder ("Order a b" quantity is equal to that of "Order b c")
   fromDiff EQ = (Order (oQuantity ab) acPrice, Nothing)
   -- Remainder of type "Order b c" ("Order b c" quantity is greater than that of "Order a b")
   fromDiff GT = (Order (oQuantity ab) acPrice, Just . Left $
                  Order (oQuantity bc - oABQtyB) (oPrice bc))
   -- Remainder of type "Order a b" ("Order b c" quantity is less than that of "Order a b")
   fromDiff LT = (Order (bToA $ oQuantity bc) acPrice, Just . Right $
                  Order (bToA (oABQtyB - oQuantity bc)) (oPrice ab))
   -- Price for AC-order
   acPrice = oPrice bc Cat.. oPrice ab
   -- The AB-order's "a" quantity converted to "b" units
   oABQtyB :: Money.Dense b
   oABQtyB = Money.exchange (oPrice ab) (oQuantity ab)
   -- Convert "b" units into "a" units
   bToA :: Money.Dense b -> Money.Dense a
   bToA = Money.exchange (Money.exchangeRateRecip $ oPrice ab)

largeQtyIdOrder :: Order base base
largeQtyIdOrder =
    Order (Money.dense' pseudoInf) Cat.id
  where
    pseudoInf = fromIntegral (maxBound :: Int64) % 1

class OrderbookSide a (base :: Symbol) (quote :: Symbol) where
    isEmpty         :: a base quote -> Bool                -- ^ No orders?
    totalBaseQty    :: a base quote -> Money.Dense base    -- ^ Total order quantity
    totalQuoteQty   :: a base quote -> Money.Dense quote   -- ^ Total order quote quantity

instance OrderbookSide BuySide base quote where
    isEmpty = null . buySide
    totalBaseQty = sum . map oQuantity . Vec.toList . buySide
    totalQuoteQty =
        sum . map (\Order{..} -> Money.exchange oPrice oQuantity) . Vec.toList . buySide

instance OrderbookSide SellSide base quote where
    isEmpty = null . sellSide
    totalBaseQty = sum . map oQuantity . Vec.toList . sellSide
    totalQuoteQty =
        sum . map (\Order{..} -> Money.exchange oPrice oQuantity) . Vec.toList . sellSide

data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: BuySide base quote
   , obAsks  :: SellSide base quote
   } deriving (Eq, Generic)

instance NFData (BuySide base quote)
instance NFData (SellSide base quote)
instance NFData (OrderBook venue base quote)

instance Cat.Category (OrderBook venue) where
   id = OrderBook Cat.id Cat.id
   (OrderBook b1 s1) . (OrderBook b2 s2) =
        OrderBook (b1 Cat.. b2) (s1 Cat.. s2)

class SellOrders a (base :: Symbol) (quote :: Symbol) where
    sellOrders :: a base quote -> Vector (Order base quote)

class BuyOrders a (base :: Symbol) (quote :: Symbol) where
    buyOrders :: a base quote -> Vector (Order base quote)

instance SellOrders SellSide base quote where sellOrders = sellSide
instance SellOrders (OrderBook venue) base quote where sellOrders = sellSide . obAsks

instance BuyOrders BuySide base quote where buyOrders = buySide
instance BuyOrders (OrderBook venue) base quote where buyOrders = buySide . obBids

data SomeBook (venue :: Symbol) = SomeBook
   { sbBids  :: Vector SomeOrder
   , sbAsks  :: Vector SomeOrder
   }

someBookOrders
    :: SomeBook venue
    -> (Vector SomeOrder, Vector SomeOrder) -- ^ (bids, asks)
someBookOrders SomeBook{..} = (sbBids, sbAsks)

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

toSomeBook
   :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
   => OrderBook venue base quote
   -> SomeBook venue
toSomeBook OrderBook{..} =
   SomeBook (map toSomeOrder (buySide obBids))
            (map toSomeOrder (sellSide obAsks))

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
   let throwBug :: a
       throwBug = error $ "SomeOrder: invalid qty/price: " ++ show so in
   Order (fromMaybe throwBug $ Money.dense soQuantity)
         (fromMaybe throwBug $ Money.exchangeRate soPrice)

toSomeOrder
   :: (KnownSymbol base, KnownSymbol quote)
   => Order base quote
   -> SomeOrder
toSomeOrder Order{..} =
   SomeOrder (toRational oQuantity)
             (Money.exchangeRateToRational oPrice)

unsafeCastOrderbook
    :: (KnownSymbol base2, KnownSymbol quote2)
    => OrderBook venue1 base1 quote1
    -> OrderBook venue2 base2 quote2
unsafeCastOrderbook OrderBook{..} =
    OrderBook (BuySide . castOrders $ buySide obBids)
              (SellSide . castOrders $ sellSide obAsks)

castOrders :: (Functor f, KnownSymbol a2, KnownSymbol b2)
            => f (Order a1 b1) -> f (Order a2 b2)
castOrders = fmap (fromSomeOrder . fromOrder)

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

showBookUgly
    :: forall venue base quote.
    ( KnownSymbol venue
    , KnownSymbol base
    , KnownSymbol quote
    )
    => OrderBook venue base quote
    -> String
showBookUgly (OrderBook (BuySide bids) (SellSide asks)) =
    let showOrderVector = intercalate " " . map showOrderUgly . Vec.toList
    in "<" ++ venue ++ " " ++ currPair ++ ", bids: " ++ showOrderVector bids ++ ", asks: " ++ showOrderVector asks ++ ">"
  where
    venue = symbolVal (Proxy :: Proxy venue)
    currPair = symbolVal (Proxy :: Proxy base) <> "/" <> symbolVal (Proxy :: Proxy quote)
    showOrderUgly :: Order src dst -> String
    showOrderUgly Order{..} =
        let doublePrice = realToFrac . Money.exchangeRateToRational $ oPrice
            doubleQty = realToFrac oQuantity
        in printf "[%f @ %f]" (doubleQty :: Double) (doublePrice :: Double)

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

instance (KnownSymbol base, KnownSymbol quote) =>
            Show (SellSide base quote) where
   show SellSide{..} =
        unlines . showOrders "SELL" . Vec.toList $ sellSide

instance (KnownSymbol base, KnownSymbol quote) =>
            Show (BuySide base quote) where
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
   hPutStr h ob = hPutStr h (toS $ show ob :: Text)
   hPutStrLn h l = hPutStr h l >> hPutStr h ("\n" :: Text)

-- | Flip a type's two "Symbol" type variables (possibly converting to a new type)
class Invertible (a :: Symbol -> Symbol -> *) (b :: Symbol -> Symbol -> *) where
    invert :: a x y -> b y x

instance Invertible Money.ExchangeRate Money.ExchangeRate where
    invert = Money.exchangeRateRecip

instance Invertible Order Order where
    invert Order{..} =
        Order (Money.exchange oPrice oQuantity) (invert oPrice)

instance Invertible SellSide BuySide where
    invert SellSide{..} =
        BuySide (fmap invert sellSide)

instance Invertible BuySide SellSide where
    invert BuySide{..} =
        SellSide (fmap invert buySide)

instance Invertible (OrderBook venue) (OrderBook venue) where
    invert OrderBook{..} =
        OrderBook (invert obAsks) (invert obBids)

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (BuySide base quote) where
    toJSON (BuySide orders) = Json.toJSON orders

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (SellSide base quote) where
    toJSON (SellSide orders) = Json.toJSON orders

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (Order base quote) where
    toJSON Order{..} = Json.object
        [ "qty"   .= Json.toJSON (toRational oQuantity)
        , "price" .= Json.toJSON (Money.exchangeRateToRational oPrice)
        ]

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (BuySide base quote) where
    parseJSON val = BuySide <$> Json.parseJSON val

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (SellSide base quote) where
    parseJSON val = SellSide <$> Json.parseJSON val

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (Order base quote) where
    parseJSON = Json.withObject "Order" $ \obj ->
        Order <$> fmap Money.dense' (obj .: "qty")
                 <*> fmap exchangeRate' (obj .: "price")
      where
        exchangeRate' r =
            fromMaybe (error $ "Bad rational: " ++ show r)
                      (Money.exchangeRate r)
