module Lib where

import MyPrelude
import GHC.TypeLits (Symbol)
import           Data.Maybe (fromMaybe)
import qualified Money
import qualified Servant.Common.BaseUrl as S
import qualified Data.Aeson   as Json
import qualified Data.Aeson.Types   as Json
import           Data.Aeson   ((.:))
import qualified Data.Scientific as Sci
import           Data.Vector  ((!), Vector)
import Control.Monad.Fail     (fail)
import Data.JSON.Schema.Types (JSONSchema(..), Schema(..), Field(..), unboundedLength)



data Order (base :: Symbol) (quote :: Symbol) = Order
   { oQuantity :: Money.Dense base
   , oPrice    :: Money.ExchangeRate base quote
   }

newtype BuyOrder  (base :: Symbol) (quote :: Symbol) = BuyOrder (Order base quote)
newtype SellOrder (base :: Symbol) (quote :: Symbol) = SellOrder (Order base quote)

data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: Vector (BuyOrder  base quote)
   , obAsks  :: Vector (SellOrder base quote)
   }

data DataSource (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = DataSource
   { dsUrl   :: S.BaseUrl
   , dsLimit :: RateLimit venue
   }

data RateLimit (venue :: Symbol) = RateLimit
   { rlPerSec      :: Word
   , rlPerSecBurst :: Word
   }

instance Json.FromJSON (OrderBook "GDAX" base quote) where
   parseJSON (Json.Object obj) =
      OrderBook <$> (fmap BuyOrder  <$> getOrders "bids" obj)
                <*> (fmap SellOrder <$> getOrders "asks" obj)
      where
            -- Schema
            getOrders :: Text -> Json.Object -> Json.Parser (Vector (Order base quote))
            getOrders key o = o .: key >>=
                  Json.withArray (toS key) (sequence . fmap getOrder)
            -- General+Schema (Scientific -> Dense/ExchangeRate)
            getOrder :: Json.Value -> Json.Parser (Order base quote)
            getOrder = Json.withArray "order" $ \vec ->
                  Order <$> parseSci "quantity" Money.dense     (vec ! 1)
                        <*> parseSci "price" Money.exchangeRate (vec ! 0)
            -- General (Scientific -> Dense/ExchangeRate)
            parseSci :: String -> (Rational -> Maybe a) -> Json.Value -> Json.Parser a
            parseSci name conv = Json.withScientific name (convSci conv)
            convSci :: (Rational -> Maybe a) -> Sci.Scientific -> Json.Parser a
            convSci conv sci = maybe (fail $ "Bad Rational: " <> show sci) return $
                           conv (toRational sci)
   parseJSON _ = fail "GDAX: Expected Object"


data GDAXBook = GDAXBook
   { gdaxBids :: Vector GDAXOrder
   , gdaxAsks :: Vector GDAXOrder
   }

instance JSONSchema GDAXBook where
   schema ob =
      Object
         [ Field { key = "bids"
                 , required = True
                 , content = Array unboundedLength False (schema $ fmap gdaxBids ob)
                 }
         , Field { key = "asks"
                 , required = True
                 , content = Array unboundedLength False (schema $ fmap gdaxAsks ob)
                 }
         ]

type GDAXOrder = (Sci.Scientific, Sci.Scientific, Text)

--instance JSONSchema GDAXOrder where
--   schema od =
--      Array (LengthBound (Just 3) (Just 3)) False ()


--data GDAXOrder = GDAXOrder
--   { price     :: Rational    -- orderVec ! 0
--   , quantity  :: Rational    -- orderVed ! 1
--   }

-- print $ (decode "[ \"295.96\",\"0.05088265\",\"3b0f1225-7f84-490b-a29f-0faef9de823a\" ]" :: Maybe (Scientific,Scientific,Text))
-- print $ (eitherDecode "[ \"295.96\",\"0.05088265\" ]" :: Either String (Scientific,Scientific))
-- print $ (eitherDecode "[ \"295.96\",\"0.05088265\" ]" :: Either String Value)

{-
GDAX:	https://api.gdax.com/products/BTC-USD/book?level=3
		https://api.gdax.com/products/BTC-EUR/book?level=3

{
    "sequence": "3",
    "bids": [
        [ price, size, order_id ],
        [ "295.96","0.05088265","3b0f1225-7f84-490b-a29f-0faef9de823a" ],
        [ "295.96","0.05088265","3b0f1225-7f84-490b-a29f-0faef9de823a" ],
        [ "295.96","0.05088265","3b0f1225-7f84-490b-a29f-0faef9de823a" ],
        ...
    ],
    "asks": [
        [ price, size, order_id ],
        [ "295.97","5.72036512","da863862-25f4-4868-ac41-005d11ab0a5f" ],
        ...
    ]
}
 -}


fetch :: DataSource venue base quote -> IO (OrderBook venue base quote)
fetch = undefined
