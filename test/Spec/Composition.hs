module Spec.Composition where

import MyPrelude                        hiding (NonEmpty)
import OrderBook
import Orphans.OrderBook                (NonEmpty(..))
import qualified Control.Category       as Cat
import qualified Data.Vector            as Vec

import Test.Hspec
import qualified Money
import qualified Test.Hspec.SmallCheck  as SC
import qualified Test.SmallCheck.Series as SS
import Test.HUnit.Lang


spec :: Spec
spec = parallel $ do
   describe "orderbook composition" $ do
      it "BuySide quantity is minimum of two composed BuySides" $
         SC.property $ \ob1 ob2 ->
            propComposeQuantityBuy (assertEqual "") ob1 ob2
      it "SellSide quantity is minimum of two composed SellSides" $
         SC.property $ \ob1 ob2 ->
            propComposeQuantitySell (assertEqual "") ob1 ob2
      it "_marketOrder oBC then _marketOrder oAB == _marketOrder (oBC . oAB)" $
         SC.property $ \o1 o2 ->
            propComposeOrderBuy assertEqual o1 o2
    --   it "marketBuy obAB then marketBuy obBC == marketBuy (obBC . obAB)" $
    --      SC.property $ \qty ob1 ob2 ->
    --         propComposeMarketBuy assertEqual qty ob1 ob2

propComposeOrderBuy
  :: (String -> Rational -> Rational -> b)
   -> Order "A" "B"
   -> Order "B" "C"
   -> b
propComposeOrderBuy compStr oAB oBC =
    let comp = compStr (unlines . intersperse "" $ [show oAC, show resBC, show resAB, show resAC])
        oAC = fst $ composeRem oBC oAB
        acQty = Money.exchange (oPrice oAC) (oQuantity oAC)
        resBC = _marketOrder (Vec.singleton oBC) acQty
        resAB = _marketOrder (Vec.singleton oAB) (resBaseQty resBC)
        resAC = _marketOrder (Vec.singleton oAC) acQty
    in  toRational (resBaseQty resAB) `comp` toRational (resBaseQty resAC)

propComposeQuantityBuy
   :: (Rational -> Rational -> b)
   -> TestOBAB
   -> TestOBBC
   -> b
propComposeQuantityBuy comp obAB obBC =
    let obAC = obBC Cat.. obAB 
        rationalBuyQty = toRational . totalQty . obBids in
    rationalBuyQty obAC `comp` min (rationalBuyQty obBC) (rationalBuyQty obAB)

propComposeQuantitySell
   :: (Rational -> Rational -> b)
   -> TestOBAB
   -> TestOBBC
   -> b
propComposeQuantitySell comp obAB obBC =
    let obAC = obBC Cat.. obAB 
        rationalSellQty = toRational . totalQty . obAsks in
    rationalSellQty obAC `comp` min (rationalSellQty obBC) (rationalSellQty obAB)

propComposeMarketBuy
   :: (String -> Money.Dense "A" -> Money.Dense "A" -> b)
   -> SS.Positive (Money.Dense "C")
   -> TestOBAB
   -> TestOBBC
   -> b
propComposeMarketBuy compStr (SS.Positive qtyC) obAB obBC =
    resBaseQty resAB `comp` resBaseQty resAC
  where
    comp = compStr (unlines [show resBC, show resAB, show resAC])
    obAC = obBC Cat.. obAB 
    resBC = marketBuy obBC qtyC
    resAB = marketBuy obAB (resBaseQty resBC)
    resAC = marketBuy obAC qtyC

type TestOBAB = OrderBook "TestVenue" "A" "B"
type TestOBBC = OrderBook "TestVenue" "B" "C"
