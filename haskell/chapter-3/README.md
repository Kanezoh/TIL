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

