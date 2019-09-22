{-# OPTIONS_GHC -fno-warn-orphans #-}
module MyPrelude
( module Protolude
, module TypeLits
, module Prelude
, sameSym
, trace
, Vector
, traceIt
, printf
)
where

import Protolude hiding (trace, Show, show)
import Prelude (String, Show, show, id, mod, lookup, error, lines, unlines)
import Debug.Trace (trace)
import GHC.TypeLits as TypeLits (Symbol, KnownSymbol, SomeSymbol(..)
                                , AppendSymbol, CmpSymbol
                                , sameSymbol, symbolVal, someSymbolVal
                                )
import           Data.Vector  (Vector)
import Text.Printf


sameSym :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
sameSym a b = isJust (sameSymbol a b)

traceIt :: Show a => a -> a
traceIt a = show a `trace` a

instance Print Rational where
   hPutStr h = let
       showDouble :: Double -> Text
       showDouble d = toS (printf "%.4g" d :: String)
       showRat :: Rational -> Text
       showRat  = showDouble . realToFrac
       in hPutStr h . showRat
   hPutStrLn h l = hPutStrLn h l >> hPutStr h ("\n" :: Text)
