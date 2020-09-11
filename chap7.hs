module Main where
  import Lib
  import Data.List

  main :: IO()

  -- 最大公約数を求める再帰関数
  myGcd a b = if remainder == 0
            then b
            else myGcd b remainder
    where remainder = a `mod` b
  -- パターンマッチング
  sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"
  
  sayAmount 1 = "one"
  sayAmount 2 = "two"
  sayAmount 3 = "a bunch"

  isEmpty [] = True
  isEmpty _ = False --アンダーバーは使用しない値を示すワイルドカード
  
  -- head関数の再実装
  myHead (x:xs) = x
  myHead [] = error "No head for empty list"

  -- tail関数の再実装
  myTail (x:xs) = xs
  myTail [] = error "No tail for empty list"

  myGcdPattenMatch a 0 = a
  myGcdPattenMatch a b = myGcdPattenMatch b (a `mod` b)

  main = do
    print (myHead [1,2,3])




