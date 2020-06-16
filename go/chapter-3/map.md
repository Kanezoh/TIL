# マップ

他言語の連想配列に当たるデータ構造。Goの場合はキーと値それぞれに型を持つ。  
~~~
// int型のキーとstring型の値を保持するマップ
var m map[int]string
~~~  

スライスと同様にmakeを使って生成できる、m[キーの値] = 要素の値という形式でもマップにキーと要素のペアを追加できる。  
~~~ 
m := make(map[int]string)

m[1] = "US"
m[81] = "Japan"
m[86] = "China"

fmt.Println(m)
=> map[1:US 81:Japan 86:China]
~~~  
キーの値が重複する代入を行うと要素の値は上書きされる。

## マップのリテラル
~~~
m := map[int]string{1: "Taro", 2: "Hanako", 3: "Jiro"}

// こんな感じで行数を分けても良い
m := map[int]string{
  1: "Taro",
  2: "Hanako",
  3: "Jiro", // ⬅︎カンマが必要
}
~~~  

## 要素の参照
演算子[]を利用してキーを指定すればそのキーの要素が参照できる。該当するキーがない場合はその型の初期値(intなら0,stringなら空文字)が返される。この挙動は意図せぬバグを産みやすいので以下のような式も使える。  
~~~
m := map[int]string{1: "A", 2: "B", 3: "C"}

s, ok := m[1] // => s == "A", ok == true
s, ok := m[9] // => s == "", ok == false
~~~  
2番目の要素にはbool型でそのキーが存在するかどうかを返す。2番目の引数の名前をokにするのはGoのイディオム。  
if文と絡めて使われる。  
~~~  
m := map[int]string{1: "A", 2: "B", 3: "C"}

if _, ok == m[1]; ok{
  // m[1]が存在する場合の処理
}
~~~  

## マップとfor
~~~
m := map[int]string {
  1: "Apple",
  2: "Banana",
  3: "Cherry",
}

for k,v = range m{
  fmt.Printf("%d => %s\n", k, v)
}
1 => Apple
2 => Banana
3 => Cherry
~~~  
スライスと違って順番は保証されないので注意！  

## len
スライスと同様、マップに格納されている要素数を整数で取得できる。  
~~~
m := map[int]string{1: "A", 2: "B", 3: "C"}  
len(m) => 3
m[4] = "D"
m[5] = "E"
len(m) => 5
~~~  
ちなみにcapはない。

## delete
マップから任意の要素を取り除くための組み込み関数。
~~~  
m := map[int]string{1: "A", 2: "B", 3: "C"}  

delete(m, 2)
fmt.Println(m)
=> map[1:A 3: C]
~~~  
該当する要素が存在しなければ何もせずに終わる、確認するまでわからないので注意。  

## 要素数に最適化したmake

makeを使ってmapを生成するときに2番目の引数に「要素数に対応した初期スペース」を整数で指定することができる。  
~~~  
m := make(map[int]string, 100)
~~~  
マップに格納される要素数を元にGoが最適なメモリ領域を確保するためのヒントとして使う。  

