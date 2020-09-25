{-# LANGUAGE BlockArguments #-}
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving Enum

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- Eq, Ordのインスタンスを自分で定義する場合
-- EqはまだしもOrdは多すぎる、データの順番に気をつけてderivingから派生させるのが良い
--instance Eq SixSidedDie where
--  (==) S6 S6 = True
--  (==) S5 S5 = True
--  (==) S4 S4 = True
--  (==) S3 S3 = True
--  (==) S2 S2 = True
--  (==) S1 S1 = True
--  (==) _ _   = False
--
--instance Ord SixSidedDie where
--  compare S6 S6 = EQ
--  compare S6 _  = GT
--  compare _ S6  = LT

-- Enumのインスタンスを自分で定義
--instance Enum SixSidedDie where
--  toEnum 0 = S1
--  toEnum 1 = S2
--  toEnum 2 = S3
--  toEnum 3 = S4
--  toEnum 4 = S5
--  toEnum 5 = S6
--  toEnum _ = error "No such value"
--
--  fromEnum S1 = 0
--  fromEnum S2 = 1
--  fromEnum S3 = 2
--  fromEnum S4 = 3
--  fromEnum S5 = 4
--  fromEnum S6 = 5

instance Eq SixSidedDie where
  (==) s1 s2 = if show s1 == show s2
               then True
               else False

instance Ord SixSidedDie where
  compare s1 s2
    | fromEnum s1 > fromEnum s2 = GT
    | fromEnum s1 == fromEnum s2 = EQ
    | otherwise = LT

newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name(f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

data FiveSidedDie = N1 | N2 | N3 | N4 | N5 deriving (Eq, Ord, Enum)
class (Eq a, Enum a) => Die a where
  roll :: Int -> a
instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
