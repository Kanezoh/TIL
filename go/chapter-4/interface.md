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

