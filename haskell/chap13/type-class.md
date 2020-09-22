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

### Bounded
似たような数字型のIntとIntegerだが、infoで型クラスを確認すると一つだけ違いが見つかる。  
IntはBounded型クラスのインスタンスだが、Integerはそうではない。  

**Bounded型クラスの定義(値は要求するが関数は要求しない)**
~~~  
class Bounded a where
  minBound :: a
  maxBound :: a
~~~  

~~~  
minBound :: Int
=> -9223372036854775808
maxBound :: Int
=> 9223372036854775807
minBound :: Char
=> '\NUL'
maxBound :: Char
=> '\1114111'
~~~  

### Show
showとread関数を可能にしている便利な型クラス。  
~~~  
class Show a where
  show :: a -> String
~~~  
ghciで値が表示できる = showを実装しているということ。
Showを実装せずに独自の型を作ってもghciで表示はできない。  
~~~  
data IceCream = Chocolate | Vanilla
~~~  

~~~  
True
=> True
False
=> False
Chocolate
=> エラー、表示できない
~~~  

### 型クラスの派生クラス
型定義の後に派生させたい型クラスを書くとそれらの派生クラスとして定義できる。  

~~~  
data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)
~~~

~~~  
Chocolate
=> Chocolate
Vanilla == Vanilla
=> True

-- Vanillaの方が後に定義したので大きいと評価される
Vanilla > Chocolate
=> True
~~~  

