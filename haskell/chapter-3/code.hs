--sumSquareOrSquareSum x y = if sumSquare > squareSum
--                           then sumSquare
--                           else squareSum
--  where sumSquare = x^2 + y^2
--        squareSum = (x + y)^2


-- Another approach

--sumSquareOrSquareSum x y = (\sumSquare squareSum ->
--                            if sumSquare > squareSum
--                            then sumSquare
 --                           else squareSum) (x^2 + y^2) ((x+y)^2)
-- クイックチェック3-2

doubleDouble x = (\x -> (x*2) * 2) x

sumSquareOrSquareSum x y = let sumSquare = (x^2 + y^2)
                               squareSum = (x+y)^2
                           in
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum
-- 変数の上書きも可能
--overwrite x = let x = 2
--              in
--                let x = 3
--                in 
--                  let x = 4
--                in
--                  x
-- クイックチェック3-2 overwrite関数をラムダ式で書き直す  
overwrite x = (\x -> 
                (\x ->
                  (\x -> x) 4
                )3
              )2
-- スコープによる変数参照の違い
x = 4
-- 引数+4を返す
add1 y = y + x

-- 引数+3を返す(上で定義した変数xは関係なし)
add2 y = (\x -> y + x)3

-- 常に3を返す(引数も上で定義した変数も参照していない)
add3 y = (\y ->
          (\x -> y + x) 1) 2


-- 3.6 練習問題  
-- 3-1 この章で作った各関数をラムダ関数化してみる
-- 1. 2倍にする関数
--(\x -> x * 2)
--2. sumSquareOrSquareSum関数
(\x y ->
  let sumSquare = (x+y)^2
      squareSum = (x^2) + (y^2)
      in
       if sumSquare > squareSum
       then sumSquare
       else squareSum
)

