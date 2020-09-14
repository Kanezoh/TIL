module Main where
  import Lib
  import Data.List

  main::IO()
  -- リストの全ての要素にaを付け加える再帰関数
  addAnA [] = []
  addAnA (x:xs) = ("a "++ x):addAnA(xs)

  -- リストの全ての要素を2乗する再帰関数
  squareAll [] = []
  squareAll (x:xs) = x^2:squareAll(xs)

  -- mapに渡す再帰関数を抽象化するとこうなる、つまりmapそのものの定義と同じ
  myMap f [] = []
  myMap f (x:xs) = (f x):myMap f xs

  -- remove、テストにパスした要素を削除
  remove f [] = []
  remove f (x:xs) = if f x
                    then remove f xs
                    else x:remove f xs
  main = do
    -- map関数、リストの各要素に対して渡された関数を実行
    print(map reverse ["dog", "cat", "moose"])
    -- filter関数、Trueになった要素だけを残す関数
    print(filter even [1,2,3,4,5])
    print(filter (\(x:xs) -> x == 'a')["apple", "banana", "avocado"])
