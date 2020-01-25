# 関数と関数型プログラミング

##関数とは

f(x) = y のように、あるxに対してyが１つに定まるもの。  
シンプルな関数  
~~~
simple x = x

simple 2 =>2
~~~  

Haskellにおける関数の3ルール  
- 全ての関数が引数を受け取らなければならない
- 全ての関数が値を返さなければならない
- 関数が同じ引数で呼び出された時は常に同じ値を返さなければならない,参照透過性  

## 関数型プログラミング

~~~
myList = [1,2,3]
myList.reverse();
newList = myList.reverse();
~~~  

以上のコードはRuby,Python,JSで有効、結果はそれぞれ  
~~~
Ruby -> [3,2,1]
Python -> None
JS -> [1,2,3]
~~~  

RubyはHaskellと同じで参照透過性を持っているが他は副作用が発生している。  
また、引数なしで関数を使う→隠れた状態にアクセスする→処理が追いにくくなる。
Haskellは全ての関数に引数、戻り値を求めているため、コードは常に透過的。  

## 変数
~~~
x = 2
x = 3 -> コンパイル不可
~~~  

## 実際に関数を作る
支払うべき金額(owed)と支払われた金額(given)を計算、givenが十分なら差額、十分でないなら0を返す
関数、calcChangeを作成する。  
~~~
calcChange owed given = if given = owed > 0
                        then given - owed
                        else 0
~~~  

問題点  
- 読みにくい、given-owedを見るたびに何が起きてるか推測しなきゃいけない
- 計算を繰り返している、リソースの無駄  

Haskellではwhere句を使ってこれを解決できる。  
~~~
calcChange owed given = if chenge > 0
                        then change 
                        else 0
  where
    change = given - owed
~~~  
