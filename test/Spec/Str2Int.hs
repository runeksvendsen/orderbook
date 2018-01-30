module Spec.Str2Int where

import Lib.Util


import MyPrelude
--import Lib.OrderBook
--import Orphans.OrderBook (NonEmpty(..))
import Test.Hspec -- .Core.Spec
--import qualified Money
--import qualified Test.QuickCheck    as QC
--import qualified Text.Show.Pretty   as P
--import qualified Test.Hspec.SmallCheck as SC
--import qualified Test.SmallCheck.Series as SS
--import Test.HUnit.Lang
--import Text.Printf
--import qualified Data.Vector  as Vec
--import Control.DeepSeq


spec :: Spec
spec =
   describe "str2Int" $
      it "expected values match" $
         mapM_ (\(str,exp) -> str2Int str `shouldBe` Right exp) expected