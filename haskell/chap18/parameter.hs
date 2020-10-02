import qualified Data.Map as Map
-- 引数を取る型
-- 他の型を引数にとって保持できるコンテナのような型を定義できる
data Box a = Box a deriving Show

-- Boxの要素を出し入れするwrap, unwrap関数も定義できる
wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- Triple 同じ3つの値として定義される型
data Triple a = Triple a a a deriving Show

-- 3D空間の点をTripleとして定義する
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- Tripleで名前を表すデータ型を定義
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- イニシャルを表す
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- 各要素にアクセスするための関数(アクセサ)
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

-- リストに変換する関数
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

-- Tripleを変換する関数
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- Listもパラメータ化された型の一種
-- 組み込みListの定義
-- data [] a = [] | a:[a]
-- Listの独自実装
-- 「List型は空か型aの別のリストと値aのコンシングである」の意味
data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- 独自のmapを定義
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

-- 複数の引数を取る型
-- タプルの型定義
-- data (,) a b = (,) a b
item1 :: (String, Int)
item1 = ("Eraser", 25)
item2 :: (String, Int)
item2 = ("Pencils", 25)
item3 :: (String, Int)
item3 = ("Pens", 13)

itemInventory :: [(String, Int)]
itemInventory = [item1, item2, item3]

-- カインド: 型の型
-- 型が取るパラメータの数を指す、調べるには:kindを使用
-- 引数の数はアスタリスクで表される、一つの場合は * -> *
-- :kind Triple
-- Triple :: * -> *
-- :kind Int
-- Int :: *

-- Data.Map
-- 他の言語におけるハッシュのようなデータ構造
-- import qualified Data.Map as Map

-- パーツ
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]
-- ID
ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

-- fromList関数
-- fromList :: Ord k => [(k, a)] -> Map k a
-- Mapの検索は二部探索で行われるため、keyはOrdのインスタンスでなければならない
organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- 検索するためのlookup関数
-- Map.lookup 7 organCatalog
