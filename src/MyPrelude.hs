{-# LANGUAGE UndecidableInstances #-}
module MyPrelude
( module Protolude
, module Safe
, module TypeLits
, Show
, show
, id
, mod
, trace
, String
, Vector
, fmapL
, printf
)
where

import Protolude hiding (trace, Show, show)
import Prelude (String, Show, show, id, mod)
import Debug.Trace (trace)
import Safe
import GHC.TypeLits as TypeLits (Symbol, KnownSymbol, symbolVal)
--import Prelude (String)
--import Orphans ()
--import Data.Text
import Control.Monad.Fail
import           Data.Vector  (Vector)
import Text.Printf
import Data.EitherR (fmapL)

--instance Show a => Print a where
--   putStr a = putStr (toS (show a) :: Text)

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
