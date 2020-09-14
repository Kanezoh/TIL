module Main where
  import Lib
  import Data.List

  main::IO()

  myLength [] = 0
  myLength xs = 1 + myLength (tail xs)

  -- パターンマッチングによるmyLengthの実装(tailを使用しない)
  -- myLength [] = 0
  -- myLength (x:xs) = 1 + myLength xs

  -- takeの再実装
  myTake _ [] = []
  myTake 0 _ = []
  myTake n (x:xs) = x:rest
    where rest = myTake (n - 1) xs
  
  finiteCycle (first:rest) = first:rest ++ [first]
  -- cycle関数の実装
  myCycle (first:rest) = (first:rest)++myCycle (first:rest)

  -- アッカーマン関数の実装
  ackermann 0 n = n + 1
  ackermann m 0 = ackermann (m - 1) 1
  ackermann m n = ackermann (m-1) (ackermann m (n-1))

  -- コラッツ予想、100京くらいまで成立するらしい
  collatz 1 = 1
  collatz n = if even n
              then collatz(n `div` 2)
              else collatz(3 * n + 1)
  
  -- reverse関数の実装
  myReverse [] = []
  myReverse (x:[]) = [x]
  myReverse (x:xs) = (myReverse xs) ++ [x]

  -- フィボナッチ関数
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 1) + fib(n - 2)
  
  main = do 
    print "ababa"
