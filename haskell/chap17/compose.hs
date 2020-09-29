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

-- Monoid 単位元を要求するセミグループ
-- クラス定義
-- class Monoid a where
--   mempty :: a          単位元
--   mappend :: a -> a    <>の定義と全く同じ
--   mconcat :: [a] -> a

-- mconcat Monoidのリストを受け取ってそれらを一つのMonoidにする
-- mempty, mappendの定義は必須
-- mconcat ["does", "this", "make", "sense"]

-- Monoidを使った確率テーブル
type Events = [String]
type Probs  = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|" , show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

-- 2つのリストの直積を求める
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  -- l2の要素ごとにl1の要素を繰り返す必要がある
  where nToAdd = length l2
        -- l1を写像して要素のコピーをnToAdd個作成
        repeatedL1 = map (take nToAdd . repeat) l1
        -- 前行で得られたリストのリストを結合
        newL1 = mconcat repeatedL1
        -- l2を無限リストにし、zipWithを使って2つのリストを結合
        cycledL2 = cycle L2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat( [x, "-", y]))

combinerProbs :: Probs -> Probs -> Probs
combinerProbs p1 p2 = cartCombine (*) p1 p2

