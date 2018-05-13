{-# LANGUAGE Arrows #-}

module Lib1
    (
    ) where

import Data.Monoid ((<>))
import Debug.Trace (trace)
import Control.Arrow

readFile2 :: String -> IO String
readFile2 p = do
  d <-readFile p
  return $ "File:" ++ d
  
-- fst だけ評価 snd はそのまま
f1 :: (Int,Int) -> (Int,Int)
f1 = first (+1)

f1' ::  (Int,Int) -> (Int,Int)
f1' = proc (x,y) -> do
  x' <- (+1) -< x
  returnA -< (x',y)

f1'' :: Kleisli IO (String,String) (String,String)
f1'' = first $ Kleisli readFile
rf1'' = runKleisli f1''

f1''' :: Kleisli IO (String,String) (String,String)
f1''' = proc (x,y) -> do
  x' <- Kleisli readFile -< x
  returnA -< (x',y)
rf1''' = runKleisli f1'''


-- snd だけ評価 fst はそのまま
f2 :: (Int,Int) -> (Int,Int)
f2 = second (+1)

f2' ::  (Int,Int) -> (Int,Int)
f2' = proc (x,y) -> do
  y' <- (+1) -< y
  returnA -< (x,y')

f2'' :: Kleisli IO (String,String) (String,String)
f2'' = second $ Kleisli readFile
rf2'' = runKleisli f2''

f2''' :: Kleisli IO (String,String) (String,String)
f2''' = proc (x,y) -> do
  y' <- Kleisli readFile -< y
  returnA -< (x,y')
rf2''' = runKleisli f2'''


-- それぞれの引数で2評価　並列1
f3 :: (Int,Int) -> (Int,Int)
f3 = (+1) *** (+2)

f3' ::  (Int,Int) -> (Int,Int)
f3' = proc (x,y) -> do
  x' <- (+1) -< x
  y' <- (+2) -< y
  returnA -< (x',y')

f3'' :: Kleisli IO (String,String) (String,String)
f3'' = Kleisli readFile *** Kleisli readFile2
rf3'' = runKleisli f3''

f3''' ::  Kleisli IO (String,String) (String,String)
f3''' = proc (x,y) -> do
  x' <- Kleisli readFile -< x
  y' <- Kleisli readFile2 -< y
  returnA -< (x',y')
rf3''' = runKleisli f3'''


-- 共通の引数で2評価 並列2
f4 :: Int -> (Int,Int)
f4 = (+1) &&& (+2)

f4' ::  Int -> (Int,Int)
f4' = proc x -> do
  x' <- (+1) -< x
  y' <- (+2) -< x
  returnA -< (x',y')

f4'' :: Kleisli IO String (String,String)
f4'' = Kleisli readFile &&& Kleisli readFile2
rf4'' = runKleisli f4''

f4''' :: Kleisli IO String (String,String)
f4''' = proc x -> do
  x' <- Kleisli readFile -< x
  y' <- Kleisli readFile2 -< x
  returnA -< (x',y')
rf4''' = runKleisli f4'''


-- Left のときだけ評価 Rightのときはそのまま
g1 :: Either Float Int -> Either String Int
g1 = left show

g1' :: Kleisli IO (Either String Int) (Either String Int)
g1' = left $ Kleisli readFile
rg1' = runKleisli g1'

-- Right のときだけ評価 Leftのときはそのまま
g2 :: Either String Int -> Either String Int
g2 = right (+1)

g2' :: Kleisli IO (Either Int String) (Either Int String)
g2' = right $ Kleisli readFile
rg2' = runKleisli g2'


-- Left でも Right でもそれぞれ用の評価 -> それぞれの型に評価
g3 :: Either Float Int -> Either String Word
g3 = show +++ fromIntegral

g3' :: Kleisli IO (Either String String) (Either String String)
g3' = Kleisli readFile +++ Kleisli readFile2
rg3' = runKleisli g3'


-- Left でも Right でもそれぞれ用の評価 -> 同じ型に評価
g4 :: Either String Int -> Word
g4 = read ||| fromIntegral

g4' :: Kleisli IO (Either String String) String
g4' = Kleisli readFile ||| Kleisli readFile2
rg4' = runKleisli g4'



-- 引数を渡して評価
h1 :: Int
h1 = app ((+1),1)

h1' :: IO String
h1' = runKleisli app $ (Kleisli readFile, "data/a.txt")


-- 最左の zeroArrow でない Arrow を評価
i1' :: Kleisli IO String String
i1' = zeroArrow <+> Kleisli readFile <+> Kleisli readFile2
ri1' = runKleisli i1'

i2' :: Kleisli IO String String
i2' = zeroArrow <+> Kleisli readFile2 <+> Kleisli readFile
ri2' = runKleisli i2'

i3' :: Kleisli IO String String
i3' = Kleisli readFile <+> zeroArrow <+> Kleisli readFile2
ri3' = runKleisli i3'

i4' :: Kleisli IO String String
i4' = Kleisli readFile2 <+> zeroArrow <+> Kleisli readFile
ri4' = runKleisli i4'




-- http://d.hatena.ne.jp/propella/20070904/p1

rep (b,d) =  (d, b:d)
f = take 5 $ loop rep 1

f' = d
  where
    d = 1:d


countdown (x, f) = (f x, cd)
    where cd 0 = [0]
          cd n = n : f (n - 1)


  
type Dialogue a b = Kleisli IO a b


calc :: Dialogue Int Int
calc = proc i -> do
  y <- calcNext -< i
  y <- calcNext -< y
  r <- calcNext -< y
  returnA -< r
  where
    calcNext :: Dialogue Int Int
    calcNext = proc y -> do
      Kleisli putStrLn -< ("整数は " <> (show y) <> " です。次の整数を入力してください。")
      y' <- (arr (+y)) <<< (arr read) <<< Kleisli (\_ -> getLine) -<< ()
      returnA -< y'


-- [TODO] 止めたくなったら終わる
calc' :: Dialogue Int Int
calc' = proc i -> do
  y <- calcNext -< i
  y <- calcNext -< y
  y <- calcNext -< y
  y <- calcNext -< y
  r <- calcNext -< y
  returnA -< r
  where
    calcNext :: Dialogue Int Int
    calcNext = proc y -> do
      Kleisli putStrLn -< ("整数は " <> (show y) <> " です。次はどうしますか?(p:足 s:引 m:掛)")
      k <- Kleisli (\_ -> getLine) -<< ()
      case head k of
        'p' -> do
          Kleisli putStrLn -< ("足します。整数を入力してください。")
          y' <- (arr (+y)) <<< Kleisli (\_ -> read <$> getLine) -<< ()
          returnA -< y'
        's' -> do
          Kleisli putStrLn -< ("引きます。整数を入力してください。")
          y' <- (arr ((-)y)) <<< Kleisli (\_ -> read <$> getLine) -<< ()
          returnA -< y'
        'm' -> do
          Kleisli putStrLn -< ("掛けます。整数を入力してください。")
          y' <- (arr (*y)) <<< Kleisli (\_ -> read <$> getLine) -<< ()
          returnA -< y'



rcalc :: IO ()
rcalc = do
  putStrLn "最初の整数を入力してください。"
  i <- read <$> getLine 
  r <- (runKleisli calc') i
  putStrLn $ "結果は " <> show r <> " です。"

