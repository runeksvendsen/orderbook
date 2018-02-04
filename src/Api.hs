module Api
( Api
, ListVenues
, ListMarkets
, SlippageBuy
, SlippageSell
, AnyVenue(..)
, AnyMarket(..)
, Market(..)
, SlippageInfo(..)
) where

import MyPrelude
import Lib.Markets.Types
import Lib.Venue
import Lib.OrderBook.Output
import Servant.API

type Api = ListVenues :<|> ListMarkets :<|> SlippageSell :<|> SlippageBuy

type ListVenues
   = "list_venues"
   :> Get '[JSON] [AnyVenue]

type ListMarkets
   = Capture "venue" Text
   :> "list_markets"
   :> Get '[JSON] [AnyMarket]

type SlippageSell
   = Capture "venue" Text
   :> "slippage_sell"
   :> Capture "market" Text
   :> QueryParam "slippage" Double
   :> Get '[JSON] SlippageInfo

type SlippageBuy
   = Capture "venue" Text
   :> "slippage_buy"
   :> Capture "market" Text
   :> QueryParam "slippage" Double
   :> Get '[JSON] SlippageInfo
