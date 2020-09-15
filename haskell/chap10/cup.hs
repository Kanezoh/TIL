module Main where
  
  -- コンストラクタ
  cup f10z = \message -> message f10z
  -- アクセサ
  getOz aCup = aCup (\f10z -> f10z)
  --let aCup = cup 6
  
  main = do
    print "ababa"
    print "ababa"


