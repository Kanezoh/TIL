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
  -- map関数、リストの各要素に対して渡された関数を実行
  main = do
    print(map reverse ["dog", "cat", "moose"])

