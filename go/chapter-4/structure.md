#　構造体

構造体: 複数の任意の型をまとめたもの。

## type
予約語type、型のエイリアスを定義することができる。  

~~~  
// int型の別名としてMyintを定義
type Myint int

var n1 myInt = 5
n2 := Myint(7)
fmt.Println(n1) // == 5
fmt.Println(n2) // == 7
~~~  

メリット  
- map[string][2]float64のような複雑な型定義にAreaMapのようなエイリアスを付ければスッキリ書ける
- リテラルをそのまま使用することができる

~~~  
type (
  IntPair [2]int
  Strings []string
  AreaMap map[string][2]float64
  IntsChannel chan []int
)

pair := IntPair{1, 2}
strs := Strings{"A", "B", "C"}
amap := AreaMap{"Tokyo": {1.11, 2.22}}
ich := make(IntsChannel)
~~~  

関数型にエイリアスを定義することもできる。  
~~~  
// intを引数にとりintを返すコールバック型
type Callback func(i int) int

func Sum(ints []int, callback Callback) int {
  var sum int
  for _, i := range ints {
    sum += 1
  }
  return callback(sum)
}

func main() {
  n := Sum(
    []int{1, 2, 3, 4, 5}
    func(i int) int {
      return i * 2
    },
  )
}
~~~  

### エイリアス型の互換性
~~~  
type T0 int
type T1 int

t0 := T0(5) // t0 == 5
i0 := int(t0) // i0 == 5

t1 := T1(8) // t1 == 8
i1 := int(t1) // i1 == 8

t0 = t1 // コンパイルエラー！
~~~  

## 構造体の定義
構造体は予約語typeを用いて新しい型を定義することで使用できる。構造体はstruct{}で囲まれた範囲で定義する。  
「structで定義した構造体にtypeで新しい型名を与える」という図式。  

~~~  
type Point struct {
  X int
  Y int
}
// こういう書き方もできる
type Point struct {
  X, Y int
}
~~~  

構造体のフィールドの値を参照するには構造体.フィールド名で指定する。 また、=を使えば値の代入もできる。  
~~~  
var pt Point
pt.X // == 0(初期値)
pt.Y = 10
pt.Y // == 10
~~~  

### 複合リテラル

構造体型に各フィールドの初期値を指定しつつ構造体を生成するためのリテラル。  
~~~
pt := Point{1, 2}
pt.X // == 1
pt.Y // == 2
~~~  
{[フィールド]: [値]}のように指定すれば順番を気にすることなく明示的に初期値を設定できる。  

~~~  
pt := Point{Y: 28}
pt.X // == 0
pt.Y // == 8
~~~  

### フィールド定義の詳細
UTF-8でエンコーディングされてるのでフィールド名は割と自由につけられる。
~~~
type Person strunct {
  ID uint
  name string
  部署 string
}
~~~  

Goの慣例では先頭が英大文字の英数字によるフィールド名が一番。  
ちなみにフィールド名の省略もできる。その場合はフィールド名=型名という定義と見なされる。  
~~~  
type T struct {
  int
  float64
  string
}

t := T{1, 3.14, "文字列"}
~~~  
また、無名のフィールドも組み込むことができる。フィールド名に「_」を与えると無名になる。  
参照や代入といった操作は不可能。ただ一つのフィールドとして確実に存在はする。  
~~~  
type T struct {
  N uint
  _ int16
  S []string
}

t := T{
  N: 12
  S: []string{"A", "B", "C"}
}
fmt.Println(t) // => "{12 0 [A B C]}
~~~  
無名フィールドは構造体のメモリ上のアラインメント調整のために使われる。普通のプログラムの範囲ではまず使わないのでひとまず覚える必要はない。

### 構造体を含む構造体
~~~
type Feed struct {
  Name string
  Amount uint
}

type Animal struct {
  Name string
  Feed feed
}
~~~  

入れ子になった構造体もまとめて初期化することができる。  
~~~  
a := Animal{
  Name: "Monkey",
  Feed: Feed{
    Name: "Banana",
    Amount: 10,
  },
}

a.Name // == "Monkey"
a.Feed.Name // == "Banana"
a.Feed.Amount // == 10
~~~  
構造体のフィールド名を省略することもできる。その場合はa.Feed.Amountのような表記をa.Amountのように表記できる。  
~~~  
type Feed struct {
  Name string
  Amount uint
}

type Animal struct {
  Name string
  Feed
}

a := Animal{
  Name: "Monkey",
  Feed: Feed{
    Name: "Banana",
    Amount: 10,
  }
}
a.Amount // ==10
~~~  
Feed型のNameフィールドにアクセスするときはa.Feed.Nameのような表記をしなければならない。フィールド名を省略できるのはあくまでもフィールド名が一意に定まる場合のみ。  
