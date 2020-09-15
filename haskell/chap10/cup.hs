module Main where
  
  -- コンストラクタ
  cup f10z = \message -> message f10z
  -- アクセサ
  getOz aCup = aCup (\f10z -> f10z)
  --let aCup = cup 6

  -- コーヒーを飲むメソッド
  drink aCup ozDrank = if ozDiff >= 0
                       then cup ozDiff
                       else cup 0
    where f10z = getOz aCup
          ozDiff = f10z - ozDrank
  
  -- 空かどうかを判定するヘルパーメソッド
  isEmpty aCup = getOz aCup == 0
  -- 畳み込みでコーヒーをすする
  -- afterManySips = foldl drink aCup [1, 1 , 1 ,1]
  

  
  main = do
    print "ababa"
    print "ababa"


