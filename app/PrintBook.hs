module Main
( main )
where

import Prelude
import qualified OrderBook.Types    as OB
import qualified Data.Vector        as Vec


asks :: OB.OrderBook "Bitfinex" "BTC" "USD"
asks = OB.sellSide $ Vec.fromList
    [ OB.Order 

    ]

main = undefined
