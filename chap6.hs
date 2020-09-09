main :: IO()
main = do
  --6.1
  --リスト
  --head: 先頭要素
  print (head [1,2,3])
  --tail: 先頭以降の要素
  print (tail [1,2,3])
  --リストを構築するための演算子コンス「:」
  print (1:2:3:4:[])
  --文字列はリストで表現されている
  print ('h':'e':'l':'l':'o':[])
  --二重引用符は単一引用符の文字のリストを表す
  --リストの要素は型が同じでなければならないため以下はエラーを起こす
  -- print "h":"ello":[]

  --6.2 遅延評価
  --範囲データからリストを構築するための構文がある
  print [1 .. 10]
  print [1,3 .. 10]
  print [1,0 .. -10]
  
  --無限個のリストも定義できる、Haskellは遅延評価を行うため(コンパイルエラーなし)
  let infinity = [1 .. ]

  --6.3 リストの関数
  --'!!演算子 リストに添字を使ってアクセスする関数
  print ([1,2,3] !! 0)
  --かっこで囲めば前置演算子として使用できる
  print ((!!) [1,2,3] 2)
  let paExample1 = (!!) "dog"
  print (paExample1 2)
  --中置演算子では右オペランドを指定すれば左オペランドを、左オペランドを指定すれば右オペランドを待機する
  let paExample2 = ("dog" !!)
  print(paExample2 2)
  let paExample3 = (!! 2)
  print(paExample3 "dog")

  --length関数 リストの長さを返す
  print (length [1 .. 20])
  print (length "quickend")

  --reverse関数 リストを逆にする
  print (reverse[1,2,3])
  print (reverse("cheese"))

  --elem関数 値とリストを引数にとってその値がリストに含まれているかどうかを判定する
  print (elem 13 [0,13 .. 100])
  print (elem 'p' "cheese")

  --take関数 数値とリストを引数にとってその数値分リストから切り出す
  print (take 5 [2,4 .. 100])
  print (take 3 "wonderful")
  --リストに含まれている要素の数より数値が大きい場合はできる限り切り出す
  print (take 10000 [1])

  --drop関数 先頭から指定された要素数を削除する
  print (drop 2 [1,2,3,4,5])
  print (drop 5 "very awesome")

  --zip関数 2つのリストを組み合わせてタプルのペアを作成できる
  print (zip [1,2,3] [4,5,6])
  print (zip "dog" "rabbit")
  print (zip ['a' .. 'f'] [1 .. ])
