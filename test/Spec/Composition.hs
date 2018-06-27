module Spec.Composition where

import MyPrelude
import OrderBook
import Orphans.OrderBook (NonEmpty(..))
import Test.Hspec
import qualified Money
import qualified Test.QuickCheck    as QC
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.SmallCheck.Series as SS
import Test.HUnit.Lang
import Text.Printf
import qualified Data.Vector  as Vec


spec :: Spec
spec = parallel $ do
   describe "orderbook composition" $ do
      it "BuySide quantity is minimum of two composed BuySides" $
         SC.property $ \ob1 ob2 ->
            propComposeQuantityBuy assertEqual ob1 ob2
      it "SellSide quantity is minimum of two composed SellSides" $
         SC.property $ \ob1 ob2 ->
            propComposeQuantitySell assertEqual ob1 ob2

propComposeQuantityBuy
   :: (a -> a -> b)
   -> TestOB
   -> TestOB
   -> b
propSellSlippageQuoteBuy ob1 ob2 =
    let obComp = ob1 . ob2 in
    totalQty (obBids obComp) `comp` min (totalQty $ obBids ob1) (totalQty $ obBids ob2)

propComposeQuantitySell
   :: (a -> a -> b)
   -> TestOB
   -> TestOB
   -> b
propComposeQuantitySell ob1 ob2 =
    let obComp = ob1 . ob2 in
    totalQty (obBids obComp) `comp` min (totalQty $ obBids ob1) (totalQty $ obBids ob2)

type TestOB = OrderBook "TestVenue" "BASE" "QUOTE"
