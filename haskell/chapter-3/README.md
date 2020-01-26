# ラムダ変数とレキシカルスコープ

##ラムダ関数

無名関数、匿名関数とも呼ばれる名前を持たない関数。  
単純に引数を返すだけの関数の場合、  
~~~
\x -> x
~~~  

と書ける。  

応用例  
~~~
sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x + y)^2
~~~  

みたいな関数は  

~~~
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x^2 + y^2) ((x+y)^2)
~~~  

みたいにも書ける(いや、where句を使った方が簡潔だけどね。)  

## ラムダからletへ

where句の代わりにlet式も使える、これを使うとwhere句の読みやすさとラムダ関数の威力を組み合わせることができる(らしい)。  

~~~
sumSquareOrSquareSum x y = let sumSquare = (x^2 + y^2)
                               squareSum = (x+y)^2
                           in
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum
~~~  

letを使えば変数の上書きも可能になる。  
~~~

overwrite x = let x = 2
              in
                let x = 3
                in 
                  let x = 4
                in
                  x
~~~  

## レキシカルスコープ

レキシカルスコープのメリットを見ていくために、JSで起きがちな悪いコード例を見ていく。  

~~~
var libraryAdd = function(a,b){
  \# varキーワードを忘れてうっかりグローバル変数としてcを作成
  c = a + b 
  return c
}

var a = 2;
var b = 3;
var c = a + b;

var d = libraryAdd(10,20);
\# cはグローバル変数を参照するため、5ではなく30になる。
console.log(c);
~~~  

JSには名前空間がないため、libraryAddがcに値を代入すると、cが見つかるまで探し続けるか、
新しくグローバル変数を作成することになる。  
これはラムダ関数によって解決できる。  
~~~
(function(){
  var a = 2;
  var b = 3;
  var c = a + b;
  var d = libraryAdd(10,20);
  console.log(c);
})()
~~~  

これで関数は内部のcを参照するようになる。  
**新しい関数を定義するたびに、新しいスコープが作成され、そのスコープ内で変数が定義される。**  
変数が使用されると、まず変数の定義をそのスコープ内で探し、なければ外側のスコープを探す、これを**レキシカルスコープ**という。  
~~~  
x = 4
add1 y = y + x

add2 y = (\x -> y + x)3


add3 y = (\y ->
          (\x -> y + x) 1) 2)
~~~  

# まとめ

- where句の代わりにletも使える
- ラムダ関数を使えば、関数をその場で定義、新しいスコープも作れるため安全  
