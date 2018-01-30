module Api.Handler where

import MyPrelude
import Lib.Markets.Types
import Lib.Fetch
import Venues (venueNames)
import qualified Network.HTTP.Client   as HTTP


listVenues :: IO [Text]
listVenues = return $ map toS venueNames

listMarkets :: HTTP.Manager -> Text -> IO [AnyMarket]
listMarkets man venue =
   case someSymbolVal (toS venue) of
      SomeSymbol (Proxy :: Proxy venue) -> do

         resE <- fetch man
         let MarketList mLst = failOnErr resE :: MarketList venue
         return $ map AnyMarket mLst

slippageSell man venue market = undefined

slippageBuy man venue market = undefined
