import Data.Array.Unboxed

-- 第一引数: インデックスの下限と上限
-- 第二引数: インデックスと値のペアからなるリスト
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]
-- zeroIndexArray ! 5 => False
-- zeroIndexArray ! 3 => True

-- 1から始まるUArrayも定義可能
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

qcArray :: UArray Int Bool
qcArray = array (0,4) [(1,True), (2,True)]