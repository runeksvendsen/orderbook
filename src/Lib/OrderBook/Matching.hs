module Lib.OrderBook.Matching where

import MyPrelude hiding (trace)
import Debug.Trace (trace)
import Lib.OrderBook.Types
import qualified Money
import qualified Data.Vector as Vec
import Text.Printf
import GHC.TypeLits.List


-- | Result of order match
data FillResult
   = Matched   -- ^ Successful match
   | InsufficientDepth           -- ^ Insufficient depth to match order
      deriving (Eq, Show)

data MatchResult (base :: Symbol) (quote :: Symbol)
   = MatchResult
      { resBaseQty  :: Money.Dense base
      , resQuoteQty :: Money.Dense quote
      , resOrders   :: [Order base quote]   -- ^ Orders filled. First order filled is last in list.
      , resRes      :: FillResult           -- ^
      } deriving Eq

newtype IgnoreFillRes a = IgnoreFillRes a deriving Show

instance Eq (IgnoreFillRes (MatchResult base quote)) where
   IgnoreFillRes mr1 == IgnoreFillRes mr2 =
      resBaseQty mr1 == resBaseQty mr2
      && resQuoteQty mr1 == resQuoteQty mr2
      && resOrders mr1 == resOrders mr2

emptyMatchRes :: MatchResult base quote
emptyMatchRes = MatchResult (fromRational 0) (fromRational 0) [] InsufficientDepth

addOrder :: MatchResult src quote -> Order src quote -> MatchResult src quote
addOrder MatchResult{..} o@Order{..} =
   MatchResult (resBaseQty+oQuantity)
               (resQuoteQty+Money.exchange oPrice oQuantity)
               (o : resOrders)
               resRes

--concat :: (KnownSymbol base, KnownSymbol quote)
--   => MatchResult base quote
--   -> MatchResult base quote
--   -> MatchResult base quote

-- | Average execution price. 'Nothing' in case of zero, infinity or notANumber
executionPrice :: MatchResult base quote -> Maybe (Money.ExchangeRate base quote)
executionPrice MatchResult{..} =
   if toRational resBaseQty == 0
      then Nothing
      else Money.exchangeRate (toRational resQuoteQty / toRational resBaseQty)

-- | Difference, in percent, between average execution price and initial execution price.
--  'Nothing' in case of empty match.
slippage :: MatchResult base quote -> Maybe Rational
slippage mr@MatchResult{..} = do
   bestPrice <- Money.fromExchangeRate . oPrice <$> lastMay resOrders
   avgPrice  <- Money.fromExchangeRate <$> executionPrice mr
   let slippageAmount = abs $ bestPrice - avgPrice
   return $ slippageAmount / bestPrice * 100

_marketOrder :: Vector (Order base quote)    -- ^ All orders from order book
             -> Money.Dense quote            -- ^ Exchange 'base' worth this 'quote'-amount
             -> MatchResult base quote       -- ^ Result
_marketOrder orders quoteAmount =
   foldl' handleOrder emptyMatchRes orders
   where
   handleOrder mr@MatchResult{..} order@Order{..}
      | resRes == Matched = mr
      | resQuoteQty + Money.exchange oPrice oQuantity < quoteAmount = addOrder mr order
      | otherwise = addOrder mr{ resRes = Matched } finalOrder
         where finalOrder  = Order finalQty oPrice
               finalQty = Money.exchange (Money.flipExchangeRate oPrice) (quoteAmount-resQuoteQty)

marketSell :: OrderBook venue base quote     -- ^ Order book
           -> Money.Dense quote              -- ^ Sell 'base' worth this 'quote'-amount
           -> MatchResult base quote         -- ^ Matched buy orders
marketSell book = _marketOrder (obBids book)

marketBuy :: OrderBook venue base quote      -- ^ Order book
          -> Money.Dense quote               -- ^ Buy 'base' worth this 'quote'-amount
          -> MatchResult base quote          -- ^ Matched sell orders
marketBuy book = _marketOrder (obAsks book)

-- | Maximum amount that can be bought/sold at given slippage
_slippageOrder :: forall base quote.
                  (KnownSymbol base, KnownSymbol quote)
               => Vector (Order base quote)        -- ^ All orders from order book
               -> Rational                         -- ^ Slippage in percent (positive for buy orders, negative for sell orders)
               -> MatchResult base quote           -- ^ Matched orders
_slippageOrder orders targetSlip =
   setMatch $ foldl' handleOrder emptyMatchRes orders
   where
   -- If we've processed all orders, and the resulting slippage matches target slippage, we say it's a match
   setMatch mr = if slippage mr == Just targetSlip then mr{ resRes = Matched } else mr
   bestPrice = Money.fromExchangeRate . oPrice $ Vec.head orders
   targetPrice  = bestPrice + targetSlip*bestPrice/100  -- Target average price
   handleOrder :: MatchResult base quote -> Order base quote -> MatchResult base quote
   handleOrder mr@MatchResult{..} order@Order{..}
      | resRes == Matched = mr
      | maybe True (<= abs targetSlip) (slippage $ addOrder mr order) = addOrder mr order
      | finalQty > 0 = addOrder finalMr (Order (fromRational finalQty) oPrice)
      | otherwise    = finalMr
         where
         finalMr  = mr{ resRes = Matched }
         finalQty = abs $ (targetPrice*toRational resBaseQty - toRational resQuoteQty) /
                          (Money.fromExchangeRate oPrice - targetPrice)

slippageSell :: forall base quote venue.
                (KnownSymbol base, KnownSymbol quote)
             => OrderBook venue base quote   -- ^ Order book
             -> Rational                     -- ^ Desired slippage (in percent) (must be positive)
             -> MatchResult base quote       -- ^ Matched buy orders
slippageSell book slip = _slippageOrder (obBids book) (-1 * slip)

slippageBuy :: forall base quote venue.
               (KnownSymbol base, KnownSymbol quote)
            => OrderBook venue base quote -- ^ Order book
            -> Rational                   -- ^ Desired slippage (in percent) (must be positive)
            -> MatchResult base quote     -- ^ Matched sell orders
slippageBuy book = _slippageOrder (obAsks book)

instance (KnownSymbol base, KnownSymbol quote) => Show (MatchResult base quote) where
   show mr@MatchResult{..} =
      let
         template = "<MatchResult %s: baseQty: %s, quoteQty: %s, orders: " <> orderIndent <> "%s>"
         fillRes = if resRes == Matched then "filled" else "unfilles" :: String
         baseQty = realToFrac (toRational resBaseQty) :: Double
         quoteQty = realToFrac $ toRational resQuoteQty :: Double
         orderIndent = "\n\t"
         orderLstStr = intercalate orderIndent (show <$> reverse resOrders)
      in printf template fillRes
                (printf "%.8g" baseQty :: String)
                (printf "%.8g" quoteQty :: String)
                orderLstStr

instance (KnownSymbol base, KnownSymbol quote) => Print (MatchResult base quote) where
   putStr mr@MatchResult{..} =
      let fillRes = if resRes == Matched then "Order filled: " else "Partial fill: "
          baseQty = realToFrac (toRational resBaseQty) :: Double
          quoteQty = realToFrac $ toRational resQuoteQty :: Double
          priceM = realToFrac . Money.fromExchangeRate <$> executionPrice mr  :: Maybe Double
          baseSymbol = symbolVal (Proxy :: Proxy base)
          quoteSymbol = symbolVal (Proxy :: Proxy quote)
          handleEmpty = maybe "<empty order>"
          showDouble :: Double -> Text
          showDouble d = toS (printf "%.4g" d :: String)
          showRat :: Rational -> Text
          showRat  = showDouble . realToFrac
          showT :: Show a => a -> Text
          showT = toS . show
          finalStr = fillRes <> showDouble baseQty <> " " <> toS baseSymbol <> " @ "
                             <> handleEmpty showDouble priceM
                             <> " (" <> showT quoteQty <> " " <> toS quoteSymbol <> ")"
                             <> " Slippage: " <> handleEmpty showRat (slippage mr) <> "%"
      in putStr (finalStr :: Text)
   putStrLn l = putStr l >> putStr ("\n" :: Text)
