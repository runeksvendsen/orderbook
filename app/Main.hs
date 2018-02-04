module Main where

import MyPrelude
import qualified Lib
import qualified Lib.OrderBook.Output as LibOut
import qualified Api.Docs
import Api.Handler
import qualified Servant.Server as SS
import Venues ()
import qualified Data.Aeson as Json
import qualified Network.HTTP.Client.TLS as HTTPS


-- getMarkets

main :: IO ()
main = do
   man <- HTTPS.newTlsManager
--   markets <- SS.runHandler $ listMarkets man "bitstamp"
--   mapM_ print markets
   either (error . toS . show) (putStrLn . Json.encode) =<<
      SS.runHandler (slipSell man "bitstamp" "BTC-USD-btcusd" 1.0)
   either (error . toS . show) (putStrLn . Json.encode) =<<
      SS.runHandler (slipBuy man "bitstamp" "BTC-USD-btcusd" 1.0)
--   markets <- getMarkets man
--   mapM_ print markets
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "gdax-l2" "BTC" "USD") -> (putStrLn book >> sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "gdax-l3" "BTC" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "bitstamp" "BTC" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "bitfinex" "BTC" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "bitfinex-v2" "USDT" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . show) return >>=
--      \(book :: Lib.OrderBook "bittrex" "ADA" "BTC") -> (putStrLn book >> sellBuy book)

sellBuy :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => Lib.OrderBook venue base quote
        -> IO ()
sellBuy book = do
   let quoteQty = 1e5
       slipPct  = 1
--   putStr ("BUY:  " :: Text)
--   putStrLn $ Lib.marketBuy book (fromRational quoteQty)
--   putStr ("SELL: " :: Text)
--   putStrLn $ Lib.marketSell book (fromRational quoteQty)
   putStr ("Slippage SELL: " :: Text)
   putStrLn $ show . LibOut.fromMatchRes $ Lib.slippageSell book (fromRational slipPct)
   putStr ("Slippage BUY:  " :: Text)
   putStrLn $ show . LibOut.fromMatchRes $ Lib.slippageBuy book (fromRational slipPct)
   putStrLn ("" :: Text)
