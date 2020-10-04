import Data.List
import Data.List.Split

myLines = splitOn "\n"
toInts :: String -> [Int]
toInts = map read . lines

main :: IO()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

-- クイックチェック3 反転して出力
-- reverseMain :: IO()
-- reverseMain = do
-- userInput <- getContents
--  let reversed = reverse userInput
--  print reversed

-- クイックチェック4 入力の2乗の総和
squareSum :: IO ()
squareSum = do
  userInput <- getContents
  let numbers = toInts userInput
  let squareNumbers = map (\x -> x^2) numbers
  print (sum squareNumbers)
