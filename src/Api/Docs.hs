{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Api.Docs where

import MyPrelude
import Api
import Venues
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.HTTP.Types
--import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server

{-
apiDocs :: API
apiDocs = docs (Proxy :: Proxy Api)

apiMarkdown :: String
apiMarkdown = markdown apiDocs


instance ToCapture (Capture "venue" Text) where
  toCapture _ =
    DocCapture "venue"                                -- name
               "(string) venue name; returned by 'list_venues'" -- description

instance ToCapture (Capture "market" Text) where
  toCapture _ =
    DocCapture "market"                                -- name
               "(string) market name; returned by 'list_markets'" -- description

instance ToSample AnyVenue  where
  toSamples _ = [ ("", AnyVenue (Proxy :: Proxy "bitfinex")) ]

instance ToSample AnyMarket where
  toSamples _ =
    [ ("", AnyMarket (Market "BTC" "USD" "btcusd" :: Market "bitfinex") )

    ]
    -- mutliple examples to display this time

instance ToParam (QueryParam "slippage" Double) where
   toParam _ =
      DocQueryParam "slippage"                     -- name
                  ["0.1", "1.0", "2.3"] -- example of values (not necessarily exhaustive)
                  "Target slippage in percent" -- description
                  Normal -- Normal, List or Flag

instance ToSample SlippageInfo where
   toSamples _ =
      [ ("", SlippageInfo
               { base_qty = 7469440144397 % 16423200000
               , quote_qty = 1821654531855684757 % 456200000000
               , init_price = Just (43488 % 5)
               , slippage_percent = Just (1 % 1)
               , orders_exhausted = False
               }
        )
      ]
-}

