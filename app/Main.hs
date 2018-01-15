module Main where

import MyPrelude
import qualified Lib
import Venues ()
import qualified Network.HTTP.Client.TLS as HTTPS


main :: IO ()
main = do
   man <- HTTPS.newTlsManager
--   Lib.fetch man >>= either (error . show) return >>=
--      \(book :: Lib.OrderBook "GDAXl2" "BTC" "USD") -> (putStrLn book >> sellBuy book)
   Lib.fetch man >>= either (error . toS . show) return >>=
      \(book :: Lib.OrderBook "GDAXl3" "BTC" "USD") -> (putStrLn book >> sellBuy book)
   Lib.fetch man >>= either (error . toS . show) return >>=
      \(book :: Lib.OrderBook "Bitstamp" "BTC" "USD") -> (putStrLn book >> sellBuy book)
   Lib.fetch man >>= either (error . toS . show) return >>=
      \(book :: Lib.OrderBook "BitfinexV2" "BTC" "USD") -> (putStrLn book >> sellBuy book)
--   Lib.fetch man >>= either (error . show) return >>=
--      \(book :: Lib.OrderBook "Bittrex" "ADA" "BTC") -> (putStrLn book >> sellBuy book)

sellBuy :: (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
        => Lib.OrderBook venue base quote
        -> IO ()
sellBuy book = do
   let quoteQty = 1e5
       slipPct  = 0.1
   putStr ("BUY:  " :: Text)
   putStrLn $ Lib.marketBuy book (fromRational quoteQty)
   putStr ("SELL: " :: Text)
   putStrLn $ Lib.marketSell book (fromRational quoteQty)
   putStr ("Slippage SELL: " :: Text)
   putStrLn $ Lib.slippageSell book (fromRational slipPct)
   putStr ("Slippage BUY:  " :: Text)
   putStrLn $ Lib.slippageBuy book (fromRational slipPct)
   putStrLn ("" :: Text)
