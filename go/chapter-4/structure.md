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

