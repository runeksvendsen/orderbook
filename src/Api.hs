module Api where

import MyPrelude
import Servant.API

type ListVenues
   = "list_venues"
   :> Get '[JSON] [Text]

type ListMarkets
   = Capture "venue" Text
   :> "list_markets"
   :> Get '[JSON] [Text]

type SlippageSell
   = Capture "venue" Text
   :> "slippage_sell"
   :> Capture "market" Text
   :> QueryParam "slippage" Double

type SlippageBuy
   = Capture "venue" Text
   :> "slippage_buy"
   :> Capture "market" Text
   :> QueryParam "slippage" Double
