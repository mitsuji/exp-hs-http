{-# LANGUAGE Arrows #-}

module Lib1
    (
    ) where

import Debug.Trace (trace)
import Control.Arrow

-- fst だけ評価 snd はそのまま
f1 :: (Int,Int) -> (Int,Int)
f1 = first (+1)

f1' ::  (Int,Int) -> (Int,Int)
f1' = proc (x,y) -> do
  x' <- (+1) -< x
  returnA -< (x',y)

f1'':: Kleisli IO (Int,Int) (Int,Int)
f1'' = first $ arr (+1)



-- snd だけ評価 fst はそのまま
f2 :: (Int,Int) -> (Int,Int)
f2 = second (+1)

f2' ::  (Int,Int) -> (Int,Int)
f2' = proc (x,y) -> do
  y' <- (+1) -< y
  returnA -< (x,y')


-- それぞれの引数で2評価　並列1
f3 :: (Int,Int) -> (Int,Int)
f3 = (+1) *** (+2)

f3' ::  (Int,Int) -> (Int,Int)
f3' = proc (x,y) -> do
  x' <- (+1) -< x
  y' <- (+2) -< y
  returnA -< (x',y')


-- 共通の引数で2評価 並列2
f4 :: Int -> (Int,Int)
f4 = (+1) &&& (+2)

f4' ::  Int -> (Int,Int)
f4' = proc x -> do
  x' <- (+1) -< x
  y' <- (+2) -< x
  returnA -< (x',y')


-- Left のときだけ評価 Rightのときはそのまま
g1 :: Either Float Int -> Either String Int
g1 = left show

-- Right のときだけ評価 Leftのときはそのまま
g2 :: Either String Int -> Either String Int
g2 = right (+1)

-- Left でも Right でもそれぞれ用の評価 -> それぞれの型に評価
g3 :: Either Float Int -> Either String Word
g3 = show +++ fromIntegral

-- Left でも Right でもそれぞれ用の評価 -> 同じ型に評価
g4 :: Either String Int -> Word
g4 = read ||| fromIntegral

-- 引数を渡して評価
h1 :: Int
h1 = app ((+1),1)




  
