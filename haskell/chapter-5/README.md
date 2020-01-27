# 5章　クロージャと部分適用

4章でifEven関数と各計算用の関数を切り出して組み合わせることを学んだ。  
~~~
ifEvenInc = ifEven inc n
ifEvenDouble = ifEven double n
ifEvenSquare = ifEven square n
~~~  

この方法も十分効率的だが、関数をまとめて定義するという作業は依然として行わなければならない。  
ifEvenxxx関数を作成する関数、genIfEven関数がほしい。  
~~~
genIfEven f = (\x -> ifEven f x) =>このラムダ関数全体を戻り値として返す
~~~  
引数fはこのラムダ関数の中でキャプチャーされる、このようにラムダ関数の中で値をキャプチャすると**クロージャ**と呼ばれるものになる。

## e.g. APIで使用するURLの生成

APIを叩くにはURLを生成する必要があり、次の４つの要素から構成される。  
1. ホスト名
2. リクエストするリソースの名前
3. リソースのID
4. APIキー  

これらからURLを生成する関数を作る  
~~~
getRequestUrl host apikey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apikey
~~~  
引数の順番と実際に使われる順番が違う？引数は出来るだけ汎用的なものを先に並べる(らしい)よ。  

~~~
getRequestUrl "http://example.com" "1337hAsk311" "book" "1234"
=> http://example.com/book/1234?token=1337Ask311
~~~  
良い感じ  
しかし、実際の現場では使うホストは限られてくることが多い。わざわざホスト名を打つのはめんどくさい。  
そこでクロージャを使って特定のホスト専用のURLビルダー関数を定義できるようにする。  
~~~
genHostRequestBuilder host = (\apikey resource id -> 
                                      getRequestUrl host apikey resource id)
〜e.g.〜
exampleUrlBuilder = genHostRequestBuilder "http://example.com"
~~~  
APIキーもなんだかんだ使う種類は限られてくるのでこれも関数化してみる。  
~~~
genApiRequestBuilder hostBuilder apikey = (\resouce id ->
                                            hostBuilder apikey resource id)
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"
~ e.g. ~
myExampleUrlBuilder "book" "1234"
~~~  
良き。  

## 部分適用

クロージャは強いけどラムダ関数を使う必要があるから可読性、検証性に疑問が残る。  
さらに、これまでは「関数に必要な引数の一部を適用して新しい関数を作成する」同じパターンだけだった。  
~~~
add4 a b c d = a + b + c + d

addXto3 x = (\b c d ->
              x + b + c + d)
addXto2 x y = (\c d ->
              x + y + c + d)
~~~  
このように、簡単な処理ですら読みにくい。Haskellにはこれに対処する機能がある。
試しにadd4に引数を１つだけ渡してみる、他の言語だとエラーになることが多いが...  
~~~
mystery = add4 3

mystery 2 3 4 => 12
mystery 5 6 7 => 21
~~~  
Haskellでは必要な引数より少ない引数だと残りの引数を待機する関数を作成する。この機能は**部分適用**と呼ばれる。  
まあつまりさっきの例で使ってためんどくさいクロージャは骨折り損だと...  

## まとめ
- クロージャを使えば新し関数を簡単に作成できる
- 部分適用でクロージャの操作が簡単になる

