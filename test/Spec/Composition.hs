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
    describe "composeRem" $ do
      it "_marketOrder oBC then _marketOrder oAB == _marketOrder (oBC . oAB)" $
         SC.property $ \o1 o2 ->
            propComposeMarketOrder assertEqual o1 o2
      it "marketBuy obBC then marketBuy obAB == marketBuy (obBC . obAB)" $
         SC.property $ \quoteQty ob1 ob2 ->
            propComposeMarketBuy assertEqual (SS.Positive quoteQty) ob1 ob2
      it "marketSell obAB then marketSell obBC == marketSell (obBC . obAB)" $
         SC.property $ \baseQty ob1 ob2 ->
            propComposeMarketSell assertEqual (SS.Positive baseQty) ob1 ob2

propComposeRemQuantity
  :: (String -> Rational -> Rational -> b)
   -> Order "A" "B"
   -> Order "B" "C"
   -> b
propComposeRemQuantity compStr oAB oBC =
    let comp = compStr (unlines . intersperse "" $ [show oAC, show resBC, show resAB, show resAC])
        oAC = fst $ composeRem oBC oAB
        cQty = min acQty bcQty
        bcQty = Money.exchange (oPrice oBC) (oQuantity oBC)
        acQty = Money.exchange (oPrice oAC) (oQuantity oAC)
        resBC = _marketOrder (Vec.singleton oBC) cQty
        resAB = _marketOrder (Vec.singleton oAB) (resBaseQty resBC)
        resAC = _marketOrder (Vec.singleton oAC) cQty
    in  toRational (resBaseQty resAB) `comp` toRational (resBaseQty resAC)

propComposeMarketOrder
  :: (String -> Rational -> Rational -> b)
   -> Order "A" "B"
   -> Order "B" "C"
   -> b
propComposeMarketOrder compStr oAB oBC =
    let comp = compStr (unlines . intersperse "" $ [show oAC, show resBC, show resAB, show resAC])
        oAC = fst $ composeRem oBC oAB
        cQty = min acQty bcQty
        bcQty = Money.exchange (oPrice oBC) (oQuantity oBC)
        acQty = Money.exchange (oPrice oAC) (oQuantity oAC)
        resBC = _marketOrder (Vec.singleton oBC) cQty
        resAB = _marketOrder (Vec.singleton oAB) (resBaseQty resBC)
        resAC = _marketOrder (Vec.singleton oAC) cQty
    in  toRational (resBaseQty resAB) `comp` toRational (resBaseQty resAC)

propComposeMarketBuy
   :: (String -> Money.Dense "A" -> Money.Dense "A" -> b)
   -> SS.Positive (Money.Dense "C")
   -> TestOBAB
   -> TestOBBC
   -> b
propComposeMarketBuy compStr (SS.Positive qtyC) obAB obBC =
    resBaseQty resAB `comp` resBaseQty resAC
  where
    minObQtyC = min (totalQuoteQty $ obAsks obBC) (totalQuoteQty $ obAsks obAC)
    actualQtyC = min qtyC minObQtyC
    obAC = obBC Cat.. obAB
    resBC = marketBuy obBC actualQtyC
    resAB = marketBuy obAB (resBaseQty resBC)
    resAC = marketBuy obAC actualQtyC
    -- Util
    comp = compStr (unlines . intersperse "" $ [show qtyC, show obBC, show obAB, show obAC, show resBC, show resAB, show resAC])

propComposeMarketSell
   :: (String -> Money.Dense "C" -> Money.Dense "C" -> b)
   -> SS.Positive (Money.Dense "A")
   -> TestOBAB
   -> TestOBBC
   -> b
propComposeMarketSell compStr (SS.Positive qtyA) obAB obBC =
    resQuoteQty resBC `comp` resQuoteQty resAC
  where
    minObQtyA = min (totalBaseQty $ obBids obAB) (totalBaseQty $ obBids obAC)
    actualQtyA = min qtyA minObQtyA
    obAC = obBC Cat.. obAB
    resAB = marketSell obAB actualQtyA
    resBC = marketSell obBC (resQuoteQty resAB)
    resAC = marketSell obAC actualQtyA
    -- Util
    comp = compStr (unlines . intersperse "" $ [show qtyA, show obBC, show obAB, show obAC, show resBC, show resAB, show resAC])

type TestOBAB = OrderBook "TestVenue" "A" "B"
type TestOBBC = OrderBook "TestVenue" "B" "C"
