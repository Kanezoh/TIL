-- クイックチェック1
-- (++) <$> Just "cat" <*> Just " and Dog"

-- pureメソッド
-- 通常の値、関数をコンテキストに配置するためのメソッド
-- pure 6 :: Maybe Int => Just 6

-- (6+) <$> Just 5 => Just 11
-- pure (6+) <*> Just 5 => Just 11

doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,2000]

-- 決定論的なコンテキスト(疑似こーど)
-- totalPrize :: Int
-- totalPrize = (+) doorPrize BoxPrize

-- 非決定論的なコンテキスト
totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- 素数のリストを返す
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)
