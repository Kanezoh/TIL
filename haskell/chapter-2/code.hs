-- シンプルな関数

simple x = x

--  練習問題
--  2.2 インクリメントするinc関数を作れ

inc n = n + 1
double n = n * 2
square n = n * n

-- 引数nが偶数の場合はn-2,奇数の場合は3*n-1を返す関数を作れ
-- 偶奇の判定はeven関数かmod関数を使え  

sample n = if result == 0
           then n - 2
           else 3 * n -2
  where
    result = mod n 2

