module Lib
    ( someFunc
    ) where

import Data.Monoid ((<>))
import Data.Word (Word8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


hex :: Integral a => a -> String
hex x = f x []
  where
    f :: Integral a => a -> String -> String
    f 0 xs = xs
    f x xs =
      let
        (d,m) = divMod x 0x10
      in  f d $ (h $ fromIntegral m) : xs

    h :: Word8 -> Char
    h 10 = 'A'
    h 11 = 'B'
    h 12 = 'C'
    h 13 = 'D'
    h 14 = 'E'
    h 15 = 'F'
    h x = head $ show x


f1 = fmap hex [1..1000]
f1' = fmap hex []
f2 = fmap hex $ Just 100
f2' = fmap hex $ Nothing
f3 = fmap hex $ Right 100
f3' = fmap hex $ Left "error!"
f4 = fmap hex (100,101)

f5 = hex <$> [1..1000]

f6 = do
--  h <- (hex . read) <$> getLine
  h <- hex <$> read <$> getLine
  putStrLn $ "0x" <> h




g1 = (+) <$> [1..10] <*> [1..10]
g1' = (pure (+)) <*> [1..10] <*> [1..10]
g1'' = (+) <$> [1..10] <*> []
g1''' = (+) <$> [] <*> [1..10]

g2 = (+) <$> Just 100 <*> Just 200
g2' = (+) <$> Just 100 <*> Nothing
g2'' = (+) <$> Nothing <*> Just 200

g3 = (+) <$> Right 100 <*> Right 200
g3' = (+) <$> Right 100 <*> Left "error!"
g3'' = (+) <$> Left "error!" <*> Right 200
g3''' = (+) <$> Left "error1!" <*> Left "error2!"

g4 = do
  p <- (+) <$> (fmap read getLine) <*> (fmap read getLine)
  putStrLn $ show p
