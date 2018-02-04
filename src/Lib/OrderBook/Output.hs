{-# LANGUAGE DeriveAnyClass #-}
module Lib.OrderBook.Output where

import MyPrelude
import Lib.OrderBook.Types
import Lib.OrderBook.Matching
import qualified Money
import Data.Aeson
import qualified Data.Scientific as Sci


data SlippageInfo = SlippageInfo
   { base_qty           :: Sci.Scientific
   , quote_qty          :: Sci.Scientific
   , init_price         :: Maybe Sci.Scientific
   , slippage_percent   :: Maybe Sci.Scientific
   , orders_exhausted   :: Bool
   } deriving (Eq, Show, Generic, FromJSON, ToJSON)

fromMatchRes
   :: MatchResult base quote
   -> SlippageInfo
fromMatchRes mr@MatchResult{..} = SlippageInfo
   { base_qty           = fromRational $ toRational resBaseQty
   , quote_qty          = fromRational $ toRational resQuoteQty
   , init_price         = fromRational . Money.fromExchangeRate . oPrice <$> headMay resOrders
   , slippage_percent   = fromRational <$> slippage mr
   , orders_exhausted   = resRes == InsufficientDepth
   }
