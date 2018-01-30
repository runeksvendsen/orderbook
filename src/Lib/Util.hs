module Lib.Util
( str2Int
, int2Str
, expected
) where

import MyPrelude
import Data.List hiding (map)
import Data.Tuple

str2Int :: String -> Either String Integer
str2Int = fmap fst . str2IntLen

str2IntLen :: String -> Either String (Integer,Int)
str2IntLen = foldr' f (Right (0,0))
   where
   f c (Right (!num,!pos)) = maybe
      (Left $ "Invalid char: '" <> [c] <> "'")
      (\ch -> Right (num + charVal ch pos, pos+1))
      (lookup c charIntMap)
   f _ left = left
   charVal :: Int -> Int -> Integer
   charVal ch' pos' = fromIntegral $ ch' * (length charIntMap^pos')

charIntMap :: [(Char, Int)]
charIntMap = zip ['A'..'Z'] [1..]

int2Str :: Integer -> String
int2Str = go "" 1
   where
--   go :: String -> Int -> Integer -> (String,Int)
   int :: (Integral a, Num b) => a -> b
   int = fromIntegral
   go !str !pos !num =
      let
          err n = "int2Str: bug: " ++ show (str,pos,num,n) in
      case fromIntegral num `divMod` length charIntMap of
         (0,0)     -> str
         (div,mod) -> maybe (error . toS $ err mod)
                    (\ch -> go (ch : str) (pos+1) (int div))
                    (lookup mod intCharMap)

intCharMap :: [(Int, Char)]
intCharMap = map swap charIntMap

expected :: [(String,Integer)]
expected =
   [ (""   , 0          )
   , ("A"  , 1          )
   , ("B"  , 2          )
   , ("Z"  , 26         )
   , ("AA" , 26+1       )
   , ("AB" , 26+2       )
   , ("AZ" , 26+26      )
   , ("BA" , 26+26+1    )
   , ("AAA", 26*26+26+1 )
   ]
