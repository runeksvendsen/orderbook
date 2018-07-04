{-# LANGUAGE IncoherentInstances #-}
module OrderBook.Matching where

import MyPrelude hiding (trace)
import Debug.Trace (trace)
import OrderBook.Types
import qualified Money
import qualified Data.Vector as Vec
import Text.Printf


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
emptyMatchRes = MatchResult (Money.dense' 0) (Money.dense' 0) [] InsufficientDepth

addOrder :: MatchResult src quote -> Order src quote -> MatchResult src quote
addOrder MatchResult{..} o@Order{..} =
   MatchResult (resBaseQty+oQuantity)
               (resQuoteQty+Money.exchange oPrice oQuantity)
               (o : resOrders)
               resRes


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
   bestPrice <- Money.exchangeRateToRational . oPrice <$> lastMay resOrders
   avgPrice  <- Money.exchangeRateToRational <$> executionPrice mr
   let slippageAmount = abs $ bestPrice - avgPrice
   return $ slippageAmount / bestPrice * 100

_marketOrder :: MarketOrder qty base quote
             => Vector (Order base quote)    -- ^ All orders from order book
             -> Money.Dense qty              -- ^ Exchange 'base' worth this 'quote'-amount
             -> MatchResult base quote       -- ^ Result
_marketOrder orders amount =
   foldl' handleOrder emptyMatchRes orders
   where
   handleOrder mr@MatchResult{..} order@Order{..}
      | resRes == Matched = mr
      | matchQuantity mr + orderQuantity order < amount = addOrder mr order
      | otherwise = addOrder mr{ resRes = Matched } finalOrder
         where finalOrder = Order finalQty oPrice
               finalQty = finalOrderQty amount mr order

class MarketOrder (qty :: Symbol) (base :: Symbol) (quote :: Symbol) where
    orderQuantity :: Order base quote -> Money.Dense qty
    matchQuantity :: MatchResult base quote -> Money.Dense qty
    finalOrderQty :: Money.Dense qty -> MatchResult base quote -> Order base quote -> Money.Dense base

instance MarketOrder quote base quote where
    orderQuantity Order{..} = Money.exchange oPrice oQuantity
    matchQuantity = resQuoteQty
    finalOrderQty amount mr Order{..} = 
        Money.exchange (Money.exchangeRateRecip oPrice) (amount-matchQuantity mr)

instance MarketOrder base base quote where
    orderQuantity = oQuantity
    matchQuantity = resBaseQty
    finalOrderQty amount mr Order{..} = amount-matchQuantity mr

marketSell :: BuyOrders bo base quote
           => bo base quote                  -- ^ Orders
           -> Money.Dense base               -- ^ Sell this "base" amount
           -> MatchResult base quote         -- ^ Matched buy orders
marketSell bo = _marketOrder (buyOrders bo)

marketBuy :: SellOrders so base quote
          => so base quote                   -- ^ Orders
          -> Money.Dense quote               -- ^ Buy "base" worth this "quote"-amount
          -> MatchResult base quote          -- ^ Matched sell orders
marketBuy so = _marketOrder (sellOrders so)

-- | Maximum amount that can be bought/sold at given slippage
_slippageOrder :: forall base quote.
                  (KnownSymbol base, KnownSymbol quote)
               => Vector (Order base quote)        -- ^ Sell/buy orders
               -> Rational                         -- ^ Slippage in percent (positive for buy orders, negative for sell orders)
               -> MatchResult base quote           -- ^ Matched orders
_slippageOrder orders targetSlip =
   setMatch $ foldl' handleOrder emptyMatchRes orders
   where
   -- If we've processed all orders, and the resulting slippage matches target slippage, we say it's a match
   setMatch mr = if slippage mr == Just targetSlip then mr{ resRes = Matched } else mr
   bestPrice = Money.exchangeRateToRational . oPrice $ Vec.head orders
   targetPrice  = bestPrice + targetSlip*bestPrice/100  -- Target average price
   handleOrder :: MatchResult base quote -> Order base quote -> MatchResult base quote
   handleOrder mr@MatchResult{..} order@Order{..}
      | resRes == Matched = mr
      | maybe True (<= abs targetSlip) (slippage $ addOrder mr order) = addOrder mr order
      | finalQty > 0 = addOrder finalMr (Order (Money.dense' finalQty) oPrice)
      | otherwise    = finalMr
         where
         finalMr  = mr{ resRes = Matched }
         finalQty = abs $ (targetPrice*toRational resBaseQty - toRational resQuoteQty) /
                          (Money.exchangeRateToRational oPrice - targetPrice)

slippageSell :: forall base quote bo.
                (KnownSymbol base, KnownSymbol quote, BuyOrders bo base quote)
             => bo base quote                -- ^ Orders
             -> Rational                     -- ^ Desired slippage (in percent) (must be positive)
             -> MatchResult base quote       -- ^ Matched buy orders
slippageSell bo slip = _slippageOrder (buyOrders bo) (-1 * slip)

slippageBuy :: forall base quote so.
               (KnownSymbol base, KnownSymbol quote, SellOrders so base quote)
            => so base quote              -- ^ Orders
            -> Rational                   -- ^ Desired slippage (in percent) (must be positive)
            -> MatchResult base quote     -- ^ Matched sell orders
slippageBuy so = _slippageOrder (sellOrders so)

instance (KnownSymbol base, KnownSymbol quote) => Show (MatchResult base quote) where
   show MatchResult{..} =
      let
         template = "<%s/%s MatchResult %s: baseQty: %s, quoteQty: %s, orders: " <> orderIndent <> "%s>"
         fillRes = if resRes == Matched then "filled" else "unfilled" :: String
         baseSymbol = symbolVal (Proxy :: Proxy base)
         quoteSymbol = symbolVal (Proxy :: Proxy quote)
         baseQty = realToFrac (toRational resBaseQty) :: Double
         quoteQty = realToFrac $ toRational resQuoteQty :: Double
         orderIndent = "\n\t"
         orderLstStr = intercalate orderIndent (showOrders "Order" $ reverse resOrders)
      in printf template 
                baseSymbol quoteSymbol fillRes
                (printf "%.8g" baseQty :: String)
                (printf "%.8g" quoteQty :: String)
                orderLstStr
