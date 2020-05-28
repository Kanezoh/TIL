# 制御構文

## for
ループ系はforのみ、forを使うと無限ループになる。  
条件を
~~~  
for i := 0; i < 10; i++ {

}
~~~  
のように指定すれば他の言語のfor文と同じように使える。  

### 裸のfor
~~~
for{
  fmt.Println("無限ループ")
}
~~~  

### break
for文,swich文,select文の内部でのみ使える。
~~~
i := 0
for{
  fmt.Println(i)
  i++

  if i == 100{
    break
  }
}
~~~  

### 条件式付きfor
~~~
i := 0
// iが100に到達するまでループ
for i < 100{
  fmt.Println(i)
  i++
}
~~~  

### 範囲説によるfor
予約語rangeと任意の式を使って構成する範囲式を使う。foreachのような構文。  
~~~  
fruits := [3]string{"Apple","Banana","Cherry"}
// rangeを伴うfor
for i,s := range fruits{
  // i: 文字列配列のインデックス
  // s: インデックスに対応した文字列の値
  fmt.Printf("fruits[%d]=%s\n", i, s)
}

=> 
fruits[0] = Apple
fruits[1] = Banana
fruits[2] = Cherry
~~~  

## if
~~~
if x == 1{

}else if y == 1{

}else{

}
~~~

### 特殊な書き方
~~~
if 簡易文; 条件式{

}
~~  

という書き方もある。簡易文は代入文や変数定義などのことで簡易文を評価してから条件式以降が評価される。  

~~~  
if x,y =  1,2; x < y{
  fmt.Printf("x(%d) is less than y(%d)\n", x, y)
}
~~~  

実用的な例
~~~
if _, err := doSomething(); err != nil {
  /エラー処理/
}
~~~  

## switch

~~~
n := 3
switch n{
  case 1,2:
    fmt.Println("1 or 2")
  case 3,4:
    fmt.Println("3 or 4")
  default:
    fmt.Println("unknown")
}
~~~  

各caseの終わりにbreakは必要ない。


## go to
関数内の任意の位置へ移動するための機能  
~~~
func main(){
  fmt.Println("A")
  goto L
  fmt.Println("B") // 処理されない
  L: // ラベル
  fmt.Println("C")
}
~~~  
**注意**  

- 関数内の任意の位置を移動するための機能なので関数間を飛ぶことは不可能
- 変数定義を飛び越えるgotoはエラー  

~~~
goto L
n := 1 // エラー
L:
fmt.Println("n=",n)
~~~  

有効な使い方は深いネストを一気に飛び越える。  
~~~
for{
    for{
        for{
          fmt.Println("start")
          goto DONE
        }
    }
}
DONE:
fmt.Println("done")
~~~  

## ラベル付き文

ラベルとラベル指定付きのbreakを使うことで前述の深いネストからの脱出を実現できる。  
~~~
LOOP:
for{
  for{
    for{
      fmt.Println("開始")
      break LOOP
    }
    fmt.Println("ここは通らない")
  }
  fmt.Println("ここは通らない")
}
~~~  

## defer  

関数の終了後に行う処理を指定することができる。deferに登録できる式は「関数呼び出し」形式の式に限られる。  
~~~  
func runDefer(){
  defet fmt.Println("defer")
  fmt.Println("done")
}
runDefer()
=>
done
defer
~~~  

defer文はいくらでも登録できるが、「より後に登録された式の方が先に評価される」ので注意が必要。  
~~~  
func runDefer(){
  defer fmt.Println("1")
  defer fmt.Println("2")
  defer fmt.Println("3")
  fmt.Println("done")
}

=>
done  
3  
2  
1  
~~~  

defer文がもっとも有効的に使われるのは「リソースの解放処理」。ファイルをオープンした後のクローズ処理などに使われる。  

~~~  
file, err := os.Open("/path/to/file")
if err != nil {
  // ファイルのオープンに失敗したらreturn
  return 
}

defer file.Close()
~~~  

## panic, recover

Goにおける例外処理みたいなもの、ただしランタイムを強制的に停止させるため多用は禁物。  

### panic
定義済み関数として以下のように定義されている。  
```func panic(v interface{})```  
実行すると即座に**ランタイムパニック**が発生、実行中の関数は中断される。  
~~~  
package main

import(
  "fmt"
)

func main(){
  panic("runtime error!") // ここでエラー終了
  fmt.Println("Hello, World")
}
~~~  
panicは例外処理ではなく「これ以上処理を継続しようがない」状態のために使われる。アプリケーションの一時的なエラー処理で使われるようなものではない。  

### recover
panicで発生したランタイムパニックによるプログラムの中断を回復するための機能。  
panicが起こっても関数内に定義されたdeferは実行されるので、defer文と組み合わせて使うのが鉄則。  

~~~  
func main(){
  defer func(){
    if x := recover(); x != nil {
      fmt.Println(x)
    }
  }()
  panic("panic!")
}
==> "panic!"
~~~  

別の使用例  
~~~  
func testRecover(src interface{}) {
  defer func() {
    if x := recover(); x != nil {
      switch v := x.(type) {
        case int:
          fmt.Printf("panic: int=%v\n", v)
        case string:
          fmt.Printf("panic: string=%v\n", v)
        default:
          fmt.Printf("panic: unknown")
      }
    }
  }()
  panic(str)
  return
}

func main(){
  testRecover(128)
  testRecover("hogehoge")
  testRecover[...]int{1,2,3})
}
=>
panic: int=128
panic: string=hogehoge
panic: unknown
~~~  

## go
並行処理を司る機能。deferと同様に関数呼び出し形式の式を引数として受け取る。  
~~~  
func sub(){
  for {
    fmt.Println("sub loop")
  }
}

func main(){
  go sub() // ゴルーチン開始
  for {
    fmt.Pritln("main loop")
  }
}
=> 
main loop と sub loop が不規則に表示される。  
~~~   

Goはスレッドより小さい処理単位のゴルーチン(go routine)が並行して動作するように実装されている。  
go文は新しくゴルーチンを生成してランタイムに追加する機能。  

~~~  
package main

import(
  "fmt"
  "runtime"
)

func main() {
  fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
  fmt.Printf("NumGoroutine: %d\n", runtime.NumGoroutine())
  fmt.Printf("Version: %s\n", runtime.Version())
}
=>
NumCPU: 2
NumGoroutine: 1
Version: go 1.6
~~~  
- NumCPU: Goランタイムが動作する環境のCPUの数  
- NumGoroutine: ランタイムで動くゴルーチンの数
- Version: Goのバージョン  

## init
パッケージを初期化するための特殊な関数
~~~  
package main

import (
  "fmt"
)

func init() {
  fmt.Println("init()")
}  

func main() {
  fmt.Println("main()")
}
=>
init()
main()
~~~  

メインルーチンが開始される前の段階で初期化処理を確実に実行するための関数。  
