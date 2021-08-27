import Data.List
import Data.Semigroup
import Data.Monoid

-- 関数合成

myLast :: [a] -> a
myLast = head . reverse

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)


-- Semigroup、同じ型を組み合わせる方法を定義する
instance Semigroup Integer where
  (<>) x y = x + y

-- Monoid、Semigroupのようなものだが、単位元を持つ
-- class Monoid a where
--   mempty :: a (単位元)
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a

-- mconcat 複数のMonoidを一度に組み合わせる
-- memptyとmappendを定義すると挙動を推測してくれる。
-- mconcat ["does", "this", "make", "sense?"] -- => "does this make sense?"

-- パラメータ型
data Box a = Box a deriving Show
wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- カインド: 型の型
-- :kind Int => Int :: *
-- :kind Box => Box :: * -> *
-- :kind Box Int => Box Int :: *
