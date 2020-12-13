-- リスト内包
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
