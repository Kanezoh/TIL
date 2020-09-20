module Main where
  halve :: Int -> Int
  halve n = n `div` 2
  printDouble :: Int -> String
  printDouble n = show (n * 2)

  main = do
  -- show 数字→文字列
  print (show 6) -- => "6"
  -- read 文字列を受け取って別の型に変換、シグネチャと併用すべき
  print (read "6" :: Int) -- => 6
  print (read "6" :: Double) -- => 6.0

