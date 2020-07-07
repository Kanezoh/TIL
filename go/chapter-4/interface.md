# インターフェース

インターフェースはGoにおける型の柔軟性を担保するための仕組み。インターフェースは型の一種であり、
「任意の方がどのようなメソッドを実装するべきか」を規定する。  
## インターフェース error
Goの組み込み型であるerrorはインターフェースとして定義されている。インターフェースはinterfaceという予約語を用いて定義する。  
interface{メソッドのシグネチャの列挙}という形。error型では文字列を返すメソッドErrorだけが定義されている。  
~~~  
type error string {
  Error() string
}
~~~  

Goにはパッケージに応じて独自のエラー型が定義されているが、これがerrorインターフェースで隠蔽されているために同じように扱うことができる。  
~~~  
func DoSomething() (int, error) {
  略
}

_, err := DoSomething()
if err != nil {
  fmt.Println(err.Error())
}
~~~  

今度はerrorインターフェースを定義したMyError型を定義してみる。構造体やメソッドの定義に任意のインターフェースを実装している
といった宣言は必要ない。  
~~~  
type MyError struct {
  Message string
  ErrCode int
}
// errorインターフェースのメソッドを実装
func (e *MyError) Error() string {
  return e.Message
}
func RaiseError() error {
  return &MyError{Message: "エラーが発生しました", ErrCode: 1234}
}

err := RaiseError()
err.Error() // == エラーが発生しました
~~~  

### インターフェースのメリット

「異なる型に共通の性質を付与することができること。」  
~~~  
type Stringify interface {
  ToString() string
}

type Person struct {
  Name string
  Age int
}

func (p *Person) ToString() string {
  return fmt.Sprintf("%s(%d)", p.Name, p.Age)
}

type Car struct {
  Number string
  Model string
}

func (c *Car) ToString() string {
  return fmt.Sprintf("[%s]%s, c.Number, c.Model)
}

vs := []Stringify{
  &Person{Name: "Taro", Age: 21},
  &Car{Number: "XXX-0123", Model: "PX512"},
}
for _, v := range vs {
  fmt.Println(v.ToString())
}
// 出力
Taro(21)
[XXX-0123] PX512
~~~  

また、インターフェースを使うことで汎用性の高い関数を定義することができる。  
~~~  
func Println(s Stringify) {
  fmt.Println(s.ToString())
}

Println(&Person{Name: "Hanako", Age: 23}) // Hanako(23)
Println(&Car{Number: "XYZ-9999", Model: "RT-38"})
~~~  
このように厳密なGoの型定義に柔軟さを持たせることができる。  

### fmt.Stringer
fmtパッケージのStringerはインターフェース。文字列を返すメソッドStringのみが定義されている。  
~~~  
type Stringer interface {
  String() string
}
~~~
T型のポインタを関数fmt.Printlnに渡すと文字列が出力される。  
~~~
type T struct {
  Id int
  Name string
}

t := &T{Id: 10, Name: "Taro"}
fmt.Println(t) // == "&{10 Taro}"
~~~  
fmt.Stringerインターフェースを活用することで任意の型の文字列表現をカスタマイズできる。  

~~~  
func (t *T) String() string {
  return fmt.Sprintf("<<%d, %s>>, t.Id, t.Name)
}
t := &T{Id: 10, Name: "Taro"}
fmt.Println(t) // == "<< 10 Taro >>"
~~~  

### インターフェースのメソッドの可視性
構造体のフィールド、メソッドの命名と同じように大文字にすればパッケージの外部から参照できる。  
インターフェースのメソッドを外部に隠蔽するメリットはないので基本的には大文字で定義すべき。  
~~~  
package foo

type I interface {
  Method1() string
  method2() string
}

type T struct{}

func (t *T) Method1() string {
  return "Method1()"
}

func (t *T) Method2() string {
  return "method2()"
}
~~~  

~~~  
package main

t := foo.New()
t.Method1() // == OK
t.Method2() // コンパイルエラー
~~~  

### インターフェースを含むインターフェース

次の例ではインターフェースを入れ子で定義しているのでインターフェースI1が要求するメソッドはMethod1とMethod2の2つになる。  
~~~  
type I0 interface {
  Method1() int
}

type I1 interface {
  I0
  Method2() int
}

type I3 interface {
  I0
  I1
  Method3() int
}
~~~  
メソッド名が重複するとエラーになるので注意が必要。  

### interface{}の本質

interfaceも構造体と同じくエイリアスを定義せずにそのまま定義できる。  
~~~  
type T struct{ Id int }

func (t *T) GetId() int { return t.Id }

func ShowId(id interface {GetId() int}) {
  fmt.Println(id.GetId())
}
~~~  

