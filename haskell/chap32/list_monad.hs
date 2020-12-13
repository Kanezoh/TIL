import Control.Monad
import Data.Char


powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  -- <-表記を使うことで、値がコンテキスト(この場合はリスト)に含まれて
  -- いないように振舞うことができる
  value <- [1 .. n]
  return (2^value)

-- mapで
-- powersOfTwo :: Int -> [Int]
-- powersOfTwo n = map (\x -> 2^x) [1..n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2^value
  let powersOfThree = 3^value
  return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
  evenValue <- [2,4 .. n]
  oddValue <- [1,3 .. n]
  return (evenValue,oddValue)

-- クイックチェック1
oneToTenSquare = do
  num <- [1 .. 10]
  let squareNum = num * num
  return (num,squareNum)

-- guard関数、filterのMonad版、条件に合わない物は排除
evenGuard :: Int -> [Int]
evenGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

-- クイックチェック2
guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter func arr = do
  value <- arr
  guard (func value)
  return value

evenSquare :: [Int]
evenSquare = do
  n <- [0 .. 9]
  let nSquared = n^2
  guard (even nSquared)
  return nSquared

-- リスト内包のpowersOfTwoAndThree
-- do表記とほぼ同じだが行はカンマ区切り
-- powersOfTwoAndThree :: [Int] -> [(Int, Int)]
-- powersOfTwoAndThree n = [ (powersOfTwo, powersOfThree)
--                         | value <- [1 .. n]
--                         , let powersOfTwo = 2^value
--                         , let powersOfThree = 3^value]

-- allEvenOdds :: Int -> [(Int, Int)]
-- allEvenOdds n = [(evenValue,oddValue) | evenValue <- [2,4 .. n]
--                                       , oddValue <- [1,3 .. n]]

-- guardもリスト内包にできる
-- evenGuard :: Int -> [Int]
-- evenGuard n = [value | value <- [1 .. n], even value]

-- クイックチェック3
answer :: [String]
answer = ["Mr. " ++ capVal
         | val <- ["brown", "blue", "pink"]
         , let capVal = (\(x: xs) -> toUpper x:xs) val]

