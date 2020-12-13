-- リスト内包
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  -- <-表記を使うことで、値がコンテキスト(この場合はリスト)に含まれて
  -- いないように振舞うことができる
  value <- [1 .. n]
  return (2^value)
