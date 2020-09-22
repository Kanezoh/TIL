# 型クラス

共通の振る舞いを持つ型のグループを表すための手段。  
~~~  
:t (+)
=> (+) :: Num a => a -> a -> a
~~~  

Numは数の概念を一般化するためのクラス。  
~~~  
:info Num
=>  
(+) :: Num a => a -> a -> a
(-) :: Num a => a -> a -> a
(*) :: Num a => a -> a -> a
...(省略)...
~~~  

:infoでこの型クラスのメンバが実装しなければならない関数のリストと、その関数の型シグネチャが表示される。  

## 型クラスの定義
~~~  
class Describable a where
  describe :: a -> String
~~~  

## よく使う型クラス

### Ord, Eq
より大きい。
~~~  
:t (>)
=> (>) :: Ord a => a -> a -> Bool
~~~  

Ordを実装している同じ型を2つとって真偽値を返している。  
Ord型クラスの定義(Eq型クラスを要求している)
~~~  
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
~~~  

Ordは何かに順番があることを示すためのクラスなので、当然それらが等しいかどうかの表現もできなければならない。
しかし、等しいことを表すためのEqクラスは順番を表現する必要はないため逆の場合は成り立たない。



