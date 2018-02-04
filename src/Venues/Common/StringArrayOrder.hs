module Venues.Common.StringArrayOrder where

import MyPrelude
import Lib.OrderBook.Types
import qualified Money
import qualified Data.Scientific as Sci
import Control.Monad.Fail
import qualified Data.Aeson.Types   as Json

type PriceStr = String
type QtyStr = String

parseOrderStr :: PriceStr -> QtyStr -> Json.Parser (Order base quote)
parseOrderStr price qty =
   Order <$> parseStr "quantity" qty Money.dense
         <*> parseStr "price" price Money.exchangeRate
   where
   parseStr name val conv = maybe (fail $ "Bad " <> name <> ": " <> val)
                                  (convSci conv) (readMaybe val)

convSci :: (Rational -> Maybe a) -> Sci.Scientific -> Json.Parser a
convSci conv sci = maybe (fail $ "Bad Rational: " <> show sci) return $
               conv (toRational sci)

parseSomeOrderStr :: PriceStr -> QtyStr -> Json.Parser SomeOrder
parseSomeOrderStr price qty = fromOrder <$> parseOrderStr price qty
