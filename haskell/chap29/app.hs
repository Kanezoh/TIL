-- クイックチェック1
-- (++) <$> Just "cat" <*> Just " and Dog"

-- pureメソッド
-- 通常の値、関数をコンテキストに配置するためのメソッド
-- pure 6 :: Maybe Int => Just 6

-- (6+) <$> Just 5 => Just 11
-- pure (6+) <*> Just 5 => Just 11

