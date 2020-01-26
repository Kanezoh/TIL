# 4章 ファーストクラス関数

**ファーストクラス関数とは？**  

他の値と同じように渡すことができる関数

## 引数としての関数

ファーストクラス関数は他の関数の引数や戻り値としてしようすることが可能。  
コードの繰り返し部分を抽象化したりできる。  
~~~
ifEvenInc n = if even n
              then n + 1
              else n
~~~  

似たような関数が必要になった場合,  
~~~
ifEvenDouble n = if even n
                 then n * 2
                 else n

ifEvenSquare n = if even n
                 then n^2
                 else n
~~~  

ここでは、偶数かどうかを判定する部分の処理は全て同じ、違うのは処理だけ。  
同じ処理を関数に切り出してみる。  
~~~
ifEven myFunction x = if even x
                      then myFunction x
                      else x
inc n = n + 1
double n = n * 2
square n = n^2
~~~  

スマート！ちなみにラムダ式も引数に渡せます。  
~~~
ifEven (\x -> x + 2) 2 => 4
~~~  

## ex)カスタムソート

~~~
author = ("Will","Kurt")
~~~  
ファーストネーム、ラストネームを対にして保存する**タプル**があるとする。  
**タプル：**複数の型を含むことができるリスト、サイズは固定。  
サイズが２のタプルにはfst,sndという関数があり、それぞれ１つ目、２つ目の要素を参照する。  
~~~
fst author => "Will"

snd author => "Kurt"
~~~  

名前のリストをソートする例を考える。  
~~~
names = [("Ian","Curtis"),
         ("Bernard","Sumner"),
         ("Peter","Hook"),
         ("Stephen","Morris")]
~~~  

便利な組み込み関数、sortは  
~~~
import Data.List
~~~  
を宣言すれば使える。でも sort namesを実行してもファーストネームによるソートになってしまう。  
ここではsortBy関数が使える。この関数は2つのタプルの比較方法を定めた関数を引数に渡せば自動で処理してくれる。  
~~~
compareLastName name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName2 > lastName1
                                    then LT
                                    else EQ
  where lastName1 = snd name1
        lastName2 = snd name2
~~~  

この関数はGT(より大きい),LT(より小さい),EQ(等しい)を返す。

## 戻り値としての関数

先ほどの例の名前のタプルと私書箱の住所を引数にとる関数を作り、手紙を出したいとする。  
住所はこんな感じ  
- 北海道札幌市
- 大阪府大阪市
- 沖縄県那覇市
~~~
addressLetter name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)
~~~  
ここで取引先から  
- 北海道では、ラストネームがアルファベットのL以降で始まるメンバーには"北海道釧路市"で送ること
- 大阪では、名前の後をハイフンではなくコロンにしてほしい
- 沖縄からは、ラストネームだけをしようしてほしい  

との連絡がきた。それぞれコードにすると
~~~
hokkaidoOffice name = if lastName < "L"
                      then nameText ++ " - 北海道札幌市"
                      else nameText ++ " - 北海道釧路市"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ (snd name)

osakaOffice name = nameText ++ " : 大阪府大阪市"
  where nameText = (fst name) ++ " " ++ (snd name)

okinawaOffice name = nameText ++ " - 沖縄県那覇市"
  where nameText = snd name
~~~  

そして、住所を受け取って正しい関数へディスパッチする関数が必要だ。  
~~~
getLocationFunc location = case location of
  "ho" -> hokkaidoOffice

  "os" -> osakaOffice

  "ok" -> okinawaOffice

  _ -> (\name -> (fst name) ++ " " ++ (snd name))
~~~  
アンダーバーはHaskellではワイルドカードとしてよく使われる。  

~~~
addressLetter name location = locationFunc name
  where locationFunc = getLocationFunc location
~~~  
これで住所によって正しい関数が返されて評価されるようになった。  

#まとめ

- ファーストクラス関数は引数になる
- ファーストクラス関数は戻り値にもなる

