module Matching where

import MyPrelude
import Lib.OrderBook
import Orphans.OrderBook (NonEmpty(..))
import Test.Hspec -- .Core.Spec
import qualified Money
import qualified Test.QuickCheck    as QC
import qualified Text.Show.Pretty   as P
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.SmallCheck.Series as SS
import Test.HUnit.Lang
import Text.Printf
import qualified Data.Vector  as Vec
--import Control.DeepSeq


spec :: Spec
spec = do
   describe "market order by slippage" $ do
      it "SELL has same MatchResult as by quote quantity" $
         SC.property $ \slippage' ob ->
            propSellSlippageQuote (assertEqArgs slippage' ob) ob slippage'
      it "BUY has same MatchResult as by quote quantity" $
         SC.property $ \slippage' ob ->
            propBuySlippageQuote (assertEqArgs slippage' ob) ob slippage'   --
   describe "init (matched orders)" $ do
      it "SELL: should be beginning of order book buy orders" $
         SC.property $ \ob qty ->
            propSellOrdersBegin shouldStartWith ob qty
      it "BUY: should be beginning of order book sell orders" $
         SC.property $ \ob qty ->
            propBuyOrdersBegin shouldStartWith ob qty
   describe "sell/buy at zero slippage" $ do
      it "SELL returns the first buy orders at same price" $
         SC.property $ \ob ->
            propSellZeroSlippage shouldBe ob
      it "BUY returns the first sell orders at same price" $
         SC.property $ \ob ->
            propBuyZeroSlippage shouldBe ob

propSellSlippageQuote
   :: (Comp -> Comp -> b)
   -> TestOB
   -> SS.NonNegative Rational
   -> b
propSellSlippageQuote comp ob (SS.NonNegative slippage') =
   let slippageRes = slippageSell ob slippage' in
   IgnoreFillRes slippageRes `comp` IgnoreFillRes (marketSell ob (resQuoteQty slippageRes))

propBuySlippageQuote
   :: (Comp -> Comp -> b)
   -> TestOB
   -> SS.NonNegative Rational
   -> b
propBuySlippageQuote comp ob (SS.NonNegative slippage') =
   let slippageRes = slippageBuy ob slippage' in
   IgnoreFillRes slippageRes `comp` IgnoreFillRes (marketBuy ob (resQuoteQty slippageRes))

propSellOrdersBegin :: ([TestOrder] -> [TestOrder] -> b) -> NonEmpty TestOB -> SS.Positive QuoteQty -> b
propSellOrdersBegin comp (NonEmpty ob) (SS.Positive qty) =
   case initMay sellRes of
      Nothing     -> error (toS errMsg) -- expectationFailure errMsg
      Just orders -> Vec.toList (fmap buyOrder (obBids ob)) `comp` orders
   where
   sellRes = reverse $ resOrders (marketSell ob qty)
   errMsg = "empty matched orders-list for positive quote quantity: " ++ show qty ++ "\n" ++ show ob

propBuyOrdersBegin :: ([TestOrder] -> [TestOrder] -> b) -> NonEmpty TestOB -> SS.Positive QuoteQty -> b
propBuyOrdersBegin comp (NonEmpty ob) (SS.Positive qty) =
   case initMay sellRes of
      Nothing     -> error (toS errMsg) -- expectationFailure errMsg
      Just orders -> Vec.toList (fmap sellOrder (obAsks ob)) `comp` orders
   where
   sellRes = reverse $ resOrders (marketBuy ob qty)
   errMsg = "empty matched orders-list for positive quote quantity: " ++ show qty ++ "\n" ++ show ob

propBuyZeroSlippage
   :: ([Order "BASE" "QUOTE"] -> [Order "BASE" "QUOTE"] -> b)
   -> NonEmpty TestOB
   -> b
propBuyZeroSlippage comp (NonEmpty ob) =
   reverse (resOrders (slippageBuy ob 0)) `comp` firstOrders
   where
   bestPrice = maybe (error $ toS errMsg) oPrice (head bookOrders)
   firstOrders = filter (\o -> oPrice o == bestPrice) bookOrders
   bookOrders = Vec.toList $ sellOrder <$> obAsks ob
   errMsg = "missing order in NonEmpty order book: " ++ show ob

propSellZeroSlippage
   :: ([Order "BASE" "QUOTE"] -> [Order "BASE" "QUOTE"] -> b)
   -> NonEmpty TestOB
   -> b
propSellZeroSlippage comp (NonEmpty ob) =
   reverse (resOrders (slippageSell ob 0)) `comp` firstOrders
   where
   bestPrice = maybe (error $ toS errMsg) oPrice (head bookOrders)
   firstOrders = filter (\o -> oPrice o == bestPrice) bookOrders
   bookOrders = Vec.toList $ buyOrder <$> obBids ob
   errMsg = "missing order in NonEmpty order book: " ++ show ob


type Comp = IgnoreFillRes (MatchResult "BASE" "QUOTE")
type TestOB = OrderBook "TestVenue" "BASE" "QUOTE"
type TestOrder = Order "BASE" "QUOTE"
type QuoteQty = Money.Dense "QUOTE"

ssGen depth = SS.list depth SS.series

assertEqArgs :: (Show a, Eq a) => SS.NonNegative Rational -> TestOB -> a -> a -> IO ()
assertEqArgs (SS.NonNegative slip) ob = assertEqual $
   printf "Slippage: %.4g, Book: \n%s"
           (realToFrac slip :: Double) (show ob)

--withParams :: SS.Serial Identity a
--           => SS.Depth
--           -> (SS.Depth -> a -> TestOB -> IO ())
--           -> IO ()
--withParams maxDepth ioa =
--   forM_ [0..maxDepth] $ \depth ->
--      forM_ (ssGen depth) $ \ob ->
--         forM_ (ssGen depth) $ \slippage' ->
--            ioa depth slippage' ob

{-

assertEqArgs :: (Show a, Eq a) => Int -> SS.NonNegative Rational -> TestOB -> a -> a -> IO ()
assertEqArgs depth (SS.NonNegative slip) ob = assertEqual $
   printf "Depth: %d, Slippage: %.4g, Book: \n%s"
           depth (realToFrac slip :: Double) (show ob)

 -}