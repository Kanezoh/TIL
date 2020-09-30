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