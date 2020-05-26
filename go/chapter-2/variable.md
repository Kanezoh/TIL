# Goの変数

## 変数の基本
### 変数定義いろいろ
int型のx,y,zを定義  

```var x,y,z int```  

int型のx,yとstring型のnameを定義  
~~~
var(
	x,y int
	name string
)
~~~  

宣言と定義を同時に  

```var x int = 1```  

暗黙的に定義(型推論)  

```i := 1 // int型のiを宣言して1を代入```  

### パッケージ変数とローカル変数
~~~
package main

import (
  "fmt"
)

// パッケージ変数(関数定義の外部で宣言された変数)
n := 100

func main(){
  n = n + 1
  fmt.Printf("n=%d\n", n) // => 101
}
~~~  

パッケージ変数は宣言されたパッケージ(今回はmainパッケージ)の中ならどこからでも参照できる。  

### 参照されない変数チェック
参照されていない変数があるとエラーが出る。  
~~~
func main(){
  a := 1
  b := 2
  c := 3

  fmt.Println(a)
}
~~~  
b,cが定義されたが使われていないのでエラーが出る。

## 基本型

### 論理値型

~~~
var b bool
b = true
~~~  

### 数値型
符号付き整数型はサイズによって  
int8, int16, int32, int64  

符号なし整数もサイズによって  
uint8, uint16, uint32, uint64  
が用意されている。これはCの場合は実装によって異なるサイズ数を明確に宣言して
実装依存を無くしている意味がある。しかし実装依存のものもあり  
int, uint, uintptr  
の3つがある。  
ちなみに64ビット実装のintとint64は全く同じだが、厳密な型チェックを行うGoでは
片方の値を片方に代入しようとするとエラーが起きる。

#### 型変換の方法

~~~  
n := 1 //int
b := byte(n) // byte型(byte型はuint8型の別名)
i64 := int64(n) //int64型
u32 := uint32(n) //uint32型  
~~~  

##### 桁あふれ
~~~
b := byte(256) // コンパイルエラー

n := 256
b := byte(n) // => 0になる
~~~  
オーバーフローが発生すると演算結果がラップアラウンドされる。

### 浮動小数点型

float32, float64  
それぞれJavaにおけるfloatとdouble

浮動小数点リテラルを暗黙的な変数定義に利用するとfloat64型が使われる。  
~~~
i := 1.0 // => float64
n := float32(1.0) // => float32
~~~  

使い分け  
**基本的にはfloat32は使用しない**

### rune型

「Unicodeコードポイントを表す特殊な整数型」、int32の別名なのでただの32ビット符号つき整数型  
~~~
r := '松'
fmt.Printf("%v", r) // => "26494"
~~~  

### 文字列型

~~~
s := "Goの文字列"
fmt.Printf("%v", s) // => Goの文字列
~~~  

#### RAW文字列リテラル
~~~
s := `
aaaaa
bbbbb
ccccc
`
~~~  

- 複数行に渡る文字列を書ける
- 改行コードの\nをそのまま出力、文字通り生の文字列として出力する  

### 配列型

~~~
a := [5]int{1,2,3,4,5}
fmt.Printf("%v", a) // => "[1,2,3,4,5]"
~~~  

{}で初期値を指定できる、{}は空でも良い。その場合の初期値は0になる。  

要素数の宣言も省略できる。  
~~~
a1 := [...]int{1,2,3} // => a1 == [3]int{1,2,3}
a2 := [...]int{1,2,3,4,5} // => a2 == [5]int{1,2,3,4,5}
a3 := [...]int{} // => a3 == [0]int{}
~~~  

配列を配列に代入する場合は要素数と型が一致していないと不可能。また、代入すると要素のコピーが作成される。  
片方の値が変わってももう片方の値に影響はない。  

~~~
a1 := [3]int{1,2,3}
a2 := [3]int{4,5,6}
a1 = a2
a1[0] = 0
a1[2] = [0]

fmt.Printf("%v\n", a1) // => "[0,5,0]"
fmt.Printf("%v\n", a2) // => "[4,5,6]"
~~~  

### interface型

全ての型と互換性のある型、JavaにおけるObjectクラスのようなもの。  
~~~
var x interface{}
fmt.Printf("%#v", x) // => "<nil>"

x = 1
x = 3.14
x = '山'
x = "文字列"
x = [...]uint8{1,2,3,4,5}
~~~  

色々な型を代入はできるが、演算の対象としては使えない。  
~~~  
var x,y interface{}
x,y = 1,2
z := x + y
~~~  

## 関数

Goにはオブジェクト指向機能がないので関数の定義と構造体の定義が中心的な作業となる。  

~~~
func plus(x,y, int) int {
  return x + y
}
plus(1,2) // => 3
~~~  

### 戻り値のない関数  
~~~
func hello(){
  fmt.Println("Hello, World!")
  return
}
~~~  
戻り値の型定義を省略すると戻り値のない関数になる。

### 複数の戻り値  
~~~
package main

import "fmt"

func div(a,b int) (int,int) {
  q := a / b
  r := a % b
  return q,r
}

func main(){
  q, r := div(19, 7)
}
~~~  

### 戻り値の破棄
_を使うと戻り値を破棄できる。  
~~~
q, _ := div(19,7)
_, r := div(19,7)

_, _ := div(19,7) // => コンパイルエラー
~~~  

### エラー処理
Goにはエラー処理がないのでエラーを表す戻り値を使ってエラーを表す。  
~~~
result, err := doSomething()
if (err != nil){
  // エラー処理
}
~~~  

### 無名関数
関数リテラルを使って名前のない関数を定義できる。  
~~~  
f := func(x,y,int) int {return x + y}
f(2,3) // => 5
~~~  

#### 関数を返す関数

~~~
package main

import (
  "fmt"
)

func returnFunc() func() {
  return func(){
    fmt.Println("I'm a function")
  }
}

func main(){
  f := returnFunc()
  f() //=> I'm a function

  returnFunc()() // => 変数を経由せずとも呼べる
}
~~~  

#### 関数を引数にとる関数
~~~
package main

import (
  "fmt"
)

func callFunction(f func()) {
  f()
}

func main(){
  callFunction(func()){
    fmt.Println("I'm a function")
  })
}
~~~  

#### クロージャ
Goの無名関数は「クロージャ」である。  
~~~  
package main

import (
  "fmt"
)

func later() func(string) string {
  var store string  // => クロージャから参照されている

  return func(next string) string {
    s := store // => 変数storeの参照
    store = next // => 変数storeへの代入
    return s
  }
}

func main(){
  f := later()

  fmt.Println(f("Golang")) // => ""
  fmt.Println(f("is")) // => "Golang"
  fmt.Println(f("good")) // => "is"
}
~~~  

通常ローカル変数は関数の終了と共に値が破棄されるが、内部的にはクロージャに属する変数として
値が保存されている。ちなみにクロージャ内で参照されていない変数に関しては値は捕捉されない。  

~~~
a := 1
b := 2 // => bだけ参照される
c := 3
return func() int {
  return b
}








