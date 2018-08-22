# Limit order book using `safe-money`

<img src="https://travis-ci.com/runeksvendsen/orderbook.svg?branch=master">

This library implements a limit order book using the `safe-money` library.

It supports two types of queries on order books:

1. **Sell/buy per quote/count currency**: e.g. for the BTCUSD market, find out how many BTC you can purchase for given amount of USD, or how many BTC you need to sell in order to earn a given USD amount
2. **Sell/buy per [slippage](https://en.wikipedia.org/wiki/Slippage_(finance))**: find out how much you can buy/sell while moving the market price up/down by the specified percentage

### About the implementation

An order book is currently represented as follows:

```haskell
data OrderBook (venue :: Symbol) (base :: Symbol) (quote :: Symbol) = OrderBook
   { obBids  :: Vector (Order base quote)
   , obAsks  :: Vector (Order base quote)
   }
```

that is, as a `Vector` of buy orders (bids) and sell orders (asks). The `OrderBook` type is parametized over -- using `Symbol`s, ie. type-level strings -- the **venue** (exchange) in question, as well as the **base** currency (the currency in which order *amounts* are quoted) and the **quote** currency (the currency in which order *prices* are quoted). 
