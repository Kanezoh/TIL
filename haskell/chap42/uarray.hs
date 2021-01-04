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

-- Intなので値は0で初期化
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

--  //演算子で値を更新する
updateBiB :: UArray Int Int
updateBiB = beansInBuckets // [(1,5), (3,6)]

-- accum関数、二項関数,UArray,UArrayに適用する値のリストを受け取る
threeTimesBiB = accum (*) updateBiB $ zip [0 .. 3] $ cycle [3]