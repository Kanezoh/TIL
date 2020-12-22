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

-- エラーのスロー
-- コンパイル時にエラーが出ないのであまり推奨されない
myHead :: [a] -> a
myHead [] = error "empty list!"
myHead (x:_) = x

-- Maybeのhead
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead(x:_) = Just x
-- (+2) <$> maybeHead [1]
-- (:) <$> maybeHead [1,2,3] <*> Just []

main :: IO ()
main = return ()
