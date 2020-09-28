import Data.List
import Data.Semigroup

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

-- セミグループ、同じ型のインスタンスを組み合わせる
instance Semigroup Integer where
  (<>) x y = x + y

-- 色を組み合わせるセミグループ
data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Green
  (<>) a b | a == b = a
           | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
           | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
           | otherwise = Brown


