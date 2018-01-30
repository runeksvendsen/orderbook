import MyPrelude

import qualified Matching as Spec
import qualified Spec.Str2Int as StrSpec
import Lib.OrderBook.Types

import Test.Tasty
import Test.Tasty.SmallCheck  as SC
import Test.Hspec             as HS
import Test.Hspec.Runner


scDepth = 7

main = do
--   hspecWith defaultConfig { configSmallCheckDepth = scDepth } Spec.spec
   hspecWith defaultConfig StrSpec.spec
   defaultMain properties

properties :: TestTree
properties = localOption (SC.SmallCheckDepth scDepth) $
   testGroup "Properties" [scProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "slippageSell ob x == marketSell ob (resQuoteQty $ slippageSell ob x)" $
      \ob slippage' -> Spec.propSellSlippageQuote (==) ob slippage'
  , SC.testProperty "slippageBuy ob x == marketBuy ob (resQuoteQty $ slippageBuy ob x)" $
      \ob slippage' -> Spec.propBuySlippageQuote (==) ob slippage'
  {-
  , SC.testProperty "obBids ob `startsWith` init (resOrders $ marketSell ob qty)" $
      \ob qty -> Spec.propSellOrdersBegin startsWith ob qty
  , SC.testProperty "obAsks ob `startsWith` init (resOrders $ marketBuy ob qty)" $
      \ob qty -> Spec.propBuyOrdersBegin startsWith ob qty
  , SC.testProperty "sell at zero slippage returns the first buy orders at same price" $
      \ob -> Spec.propSellZeroSlippage (==) ob
  , SC.testProperty "buy at zero slippage returns the first sell orders at same price" $
      \ob -> Spec.propBuyZeroSlippage (==) ob
   -}
  ]
  where a `startsWith` b = take (length b) a == b
