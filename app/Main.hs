module Main where

import MyPrelude
import qualified Lib
import Venues ()
import qualified Network.HTTP.Client.TLS as HTTPS

-- getMarkets

main :: IO ()
main = do
   man <- HTTPS.newTlsManager
--   markets <- getMarkets man
--   mapM_ print markets
   Lib.fetch man >>= either (error . toS . show) return >>=
      \(book :: Lib.OrderBook "gdax-l2" "BTC" "USD") -> (putStrLn book >> sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "gdax-l3" "BTC" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "bitstamp" "BTC" "USD") -> (sellBuy book)
--   Lib.fetch man >>= either (error . toS . show) return >>=
--      \(book :: Lib.OrderBook "bitfinex-v2" "BTC" "USD") -> (sellBuy book)
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
   putStrLn $ Lib.slippageSell book (fromRational slipPct)
   putStr ("Slippage BUY:  " :: Text)
   putStrLn $ Lib.slippageBuy book (fromRational slipPct)
   putStrLn ("" :: Text)
