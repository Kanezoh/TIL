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
