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

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

-- Maybeを使うのはいいが、Nothingが何を返すのか曖昧
-- そもそも意図してない入力なのか、入力は意図しているが単に失敗したのか
-- そのためにEither型を使う
-- data Either a b = Left a | Right b
-- Leftが失敗、Rightが成功
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1,2,3]
-- eitherHead intExample => Right 1

intExampleEmpty :: [Int]
intExampleEmpty = []
-- eitherHead intExampleEmpty => Left "There is no head because the list is empty"

charExample :: [Char]
charExample = "cat"
-- eitherHead charExample => Right 'c'

charExampleEmpty :: [Char]
charExampleEmpty = []
-- eitherHead charExampleEmpty => Left "There is no head because the list is empty"

data PrimeError = TooLarge | InvalidValue
instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

-- 複数のエラー型を指定することができる
maxN = 10
primes = [2,3,5,7]
isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

main :: IO ()
main = return ()
