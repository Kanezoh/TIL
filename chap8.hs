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
  myCycle (first:rest) = (first:rest)++myCycle (first:rest)

  ackermann 0 n = n + 1
  ackermann m 0 = ackermann (m - 1) 1
  ackermann m n = ackermann (m-1) (ackermann m (n-1))
  

  main = do 
    print "ababa"
