module Lib.Markets.Parse where


import MyPrelude
import Lib.Markets.Types
import Lib.Fetch
import Lib.OrderBook.Types
import Lib.Venue
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


-- | E.g. "BTC-USD-tBTCUSD".
--    'miApiSymbol' can contain any number of hyphens; 'miBase' and 'miQuote' cannot.
toString
   :: AnyMarket
   -> String   -- ^ Market name
toString (AnyMarket Market{..}) =
   printf "%s-%s-%s" (toS miBase :: String)
                     (toS miQuote :: String)
                     (toS miApiSymbol :: String)

instance Json.ToJSON AnyMarket where
   toJSON am = Json.toJSON (toS $ toString am :: Text)

fromString
   :: AnyVenue   -- ^ Venue name
   -> Text   -- ^ Market name
   -> Maybe AnyMarket
fromString anyVenue marketName =
   case anyVenue of
      AnyVenue (venue :: Proxy venue) -> do
         (base,quote,apiSymbol) <- parseMarketString marketName
         Just $ AnyMarket (Market (toS base) (toS quote) (toS apiSymbol) :: Market venue)

parseMarketString :: Text -> Maybe (String,String,String)
parseMarketString marketStr = do
   let noEmptyLst lst = if null lst then Nothing else Just lst
       marketName = toS marketStr
   base <- noEmptyLst $ takeWhile (/= '-') marketName
   quote <- noEmptyLst $ takeWhile (/= '-') (drop (length base + 1) marketName)
   let symbol = drop (length base + length quote + 2) marketName
   return (base,quote,symbol)
