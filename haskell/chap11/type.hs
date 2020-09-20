module Main where
  halve :: Int -> Int
  halve n = n `div` 2

  -- Int→Stringの型変換
  printDouble :: Int -> String
  printDouble n = show (n * 2)

  -- 複数の引数の型シグネチャ
  makeAddress :: Int -> String -> String -> (Int, String, String)
  makeAddress number street town = (number, street, town)

  -- ファーストクラス関数の型
  -- 関数の部分は丸括弧で引数と返り値を囲む
  ifEven :: (Int -> Int) -> Int -> Int
  ifEven f n = if even n
               then f n
               else n
  
  -- 型変数
  -- 型を特に指定しない場合は変数を置くことで勝手に置き換わる
  simple :: a -> a
  simple x = x
  main = do
  -- show 数字→文字列
  print (show 6) -- => "6"
  -- read 文字列を受け取って別の型に変換、シグネチャと併用すべき
  print (read "6" :: Int) -- => 6
  print (read "6" :: Double) -- => 6.0

