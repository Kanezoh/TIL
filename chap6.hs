main :: IO()
main = do
  --6.1--
  --リスト--
  --head: 先頭要素--
  print (head [1,2,3])
  --tail: 先頭以降の要素--
  print (tail [1,2,3])
  --リストを構築するための演算子コンス「:」--
  print (1:2:3:4:[])
  --文字列はリストで表現されている--
  print ('h':'e':'l':'l':'o':[])
  --二重引用符は単一引用符の文字のリストを表す--
  --リストの要素は型が同じでなければならないため以下はエラーを起こす--
  -- print "h":"ello":[] --

