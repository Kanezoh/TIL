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
data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Clear deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear any = any
  (<>) any Clear = any
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
instance Monoid Color where
  mempty = Clear
  mappend col1 col2 = col1 <> col2

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
data Events = Events [String]
data Probs  = Probs [Double]

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
        cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where combiner = (\x y -> mconcat( [x, "-", y]))

instance Semigroup Events where
  (<>) = combineEvents
instance Monoid Events where
  mappend = (<>)
  mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs(cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs
instance Monoid Probs where
  mappend = (<>)
  mempty = Probs []

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1 -- 空の場合
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs  = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["head", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]
