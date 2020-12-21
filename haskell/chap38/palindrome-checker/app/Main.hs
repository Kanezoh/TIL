module Main where

import Lib

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

-- コンパイル時に警告が出る書き方(-Wallオプションを付けている場合)
-- 空のリストに対するパターンマッチングが必要
-- myTakePM 0 _ [] = []
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

main :: IO ()
main = return ()
