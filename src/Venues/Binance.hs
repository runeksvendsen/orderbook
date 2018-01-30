module Venues.Binance where

import MyPrelude
import Lib.Markets
import Lib.OrderBook

-- https://api.binance.com/api/v1/exchangeInfo

newtype ExchangeInfo = ExchangeInfo
   { symbols :: [BSymbol]
   }

data BSymbol = BSymbol
    { symbol               :: Text  -- e.g. "ETHBTC"
    , status               :: Text  -- =? "TRADING"
    , baseAsset            :: Text
    , baseAssetPrecision   :: Word
    , quoteAsset           :: Text
    , quotePrecision       :: Word
    }

fromSymbol :: BSymbol -> Market "Binance"
fromSymbol BSymbol{..} = Market baseAsset quoteAsset symbol
