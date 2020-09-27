import Data.List

-- composeと呼ばれる高階関数。 .で関数を合成できる
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- リストの全ての要素で、ある特性がTrueかどうかテスト
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = foldr (&&) True . map testFunc

-- リストの少なくとも一つの要素で、ある特性がTrueかどうか
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = foldr (||) True . map testFunc
