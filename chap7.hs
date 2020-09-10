module Main where
  import Lib
  import Data.List

  main :: IO()

  -- 最大公約数を求める再帰関数
  myGcd a b = if remainder == 0
            then b
            else myGcd b remainder
    where remainder = a `mod` b
