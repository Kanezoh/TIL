import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

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

-- 値をインプレース(その場、コピー等を使用しない)で変更するにはSTUArrayが必要
-- Int型のリストをSTUArrayに変更する
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals -1
  myArray <- newArray (0,end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- STUArrayのコンテキストからUArrayを取り出す
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals -1
  myArray <- newArray (0,end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- バブルソート
-- 元データ
myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray