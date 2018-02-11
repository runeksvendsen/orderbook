# Limit order book using `safe-money`

This library implements a limit order book using the `safe-money` library.

It supports two types of queries on order books:

1. **Sell/buy per quote/count currency**: e.g. for the BTCUSD market, find out how many BTC you can purchase for given amount of USD, or how many BTC you need to sell in order to earn a given USD amount
2. **Sell/buy per [slippage](https://en.wikipedia.org/wiki/Slippage_(finance))**: find out how much you can buy/sell while moving the market price up/down by the specified percentage
