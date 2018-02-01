{-# LANGUAGE UndecidableInstances #-}
module MyPrelude
( module Protolude
, module Safe
, module TypeLits
, module Prelude
, sameSym
--, Show
--, show

--, id
--, mod
, trace
--, String
, Vector
, fmapL
, printf
, failOnErr
)
where

import Protolude hiding (trace, Show, show)
import Prelude as Prelude (String, Show, show, id, mod, lookup)
import Debug.Trace (trace)
import Safe
import GHC.TypeLits as TypeLits (Symbol, KnownSymbol, SomeSymbol(..)
                                , sameSymbol, symbolVal, someSymbolVal
                                )
--import Prelude (String)
--import Orphans ()
--import Data.Text
import Control.Monad.Fail
import           Data.Vector  (Vector)
import Text.Printf
import Data.EitherR (fmapL)
import qualified Servant.Common.Req    as Req

sameSym :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
sameSym a b = isJust (sameSymbol a b)

failOnErr :: forall a venue. KnownSymbol venue => Either Req.ServantError (a venue) -> a venue
failOnErr = either (error . toS . errMsg . show) id
   where errMsg str = symbolVal (Proxy :: Proxy venue) <> ": " <> str



toNum :: String -> Maybe Int
toNum "one"   = Just 1
toNum "two"   = Just 2
toNum "three" = Just 3
toNum _       = Nothing

numList =
   [("one",   1)
   ,("two",   2)
   ,("three", 3)
   ]

toNum' :: String -> Maybe Int
toNum' str = lookup str numList



instance Print Rational where
   putStr = let
       showDouble :: Double -> Text
       showDouble d = toS (printf "%.4g" d :: String)
       showRat :: Rational -> Text
       showRat  = showDouble . realToFrac
       in putStr . showRat
   putStrLn l = putStr l >> putStr ("\n" :: Text)


instance MonadFail (Either Text) where
  fail = Left . toS
