# チャネル

ゴルーチンとゴルーチンの間でデータの受け渡しをするためのデータ構造。  
ゴルーチンによる非同期処理以外では必要ない。  

## チャネルの型
チャネルはchan [型]のように定義する。
~~~
// 変数chをint型のチャネルとして定義する
var ch chan int
~~~  

また、受信専用のチャネルと送信専用のチャネルがあり、以下のように定義する。  
~~~  
// 受信専用
var ch1 <-chan int

// 送信専用
vat ch2 chan<- int
~~~  

## チャネルの生成と送受信

スライス、マップと同様にチャネルもmakeで生成できる。
makeへ2番目の引数を指定することでバッファのサイズを指定できる、指定がない場合は0。  
~~~
ch := make(chan int)

ch := make(chan int, 8)
~~~  
チャネルはキュー構造をしており、バッファサイズはこのキューのサイズと言える。キューはFIFO(先入先出し)の性質があり、チャネルも同様。  
チャネルが保持するデータに対する操作は送信、受信の2パターン。送受信には演算子 <- を使用します。  
~~~  
ch := make(chan int, 10)

// チャネルに整数5を送信
ch <- 5

// チャネルから整数値を受信
i := <-ch
~~~  
<の向きでデータをやりとりしている向きがわかる。  

## チャネルとゴルーチン
チャネルはキューとしてのデータ構造を持つが、単純なキューとして使えるようにデザインされていない。  

~~~  
func main() {
  ch := make(chan int)
  fmt.Println(<-ch)
}
~~~  
これを実行すると、  
```fatal error: all goroutines are asleep - deadlock!```  
のエラーが出る。上のコードはチャネルから受信する処理だが、実行時に存在するのは関数mainを処理するゴルーチン一つだけ。  
このゴルーチンが受信を待つために眠ったものの、他にデータを送信してくれるチャネルがないためにデッドロックを検知したと言う理屈。  

チャネルはゴルーチン間でデータを処理するための仕組みであり、複数のゴルーチンが必要になる。  
~~~  
package main

import(
  "fmt"
)

func receiver(ch <-chan int) {
  for {
    i := <- ch
    fmt.Println(i)
  }
}

func main() {
  ch := make(chan int)

  go receiver(ch)

  i := 0
  for i < 5 {
    ch <- i
    i ++
  }
}
~~~  
上記のコードではmain関数のゴルーチンがreceiver側のゴルーチンに値を送信し続け、receiver側ではchに渡された引数をひたすら出力し続ける。  
チャネルの処理を行なっているゴルーチンが停止するかどうかはバッファサイズにもよる。  
~~~  
ch := make(chan rune, 3)

ch <- 'A'
ch <- 'B'
ch <- 'C'
ch <- 'D' // デッドロック発生
~~~  
ゴルーチンが停止する条件は  
- バッファサイズが0、またはバッファ内がからのチャネルからの受信
- バッファサイズが0、またはバッファ内に空きがないチャネルへの送信
の２パターン。  

## len
チャネルのバッファ内に溜められているデータの数を取得できる。  
~~~  
ch := make(chan string, 3)

ch <- "Apple"
len(ch) // == 1
ch <- "Banana" 
len(ch) // == 2
~~~  

## cap
チャネルのバッファサイズを取得できる。
~~~  
ch := make(chan string)
cap(ch) // == 0

ch := make(chan string, 3)
cap(ch) // == 3
~~~  

## close
チャネルは**クローズ(closed)**という状態を持っている。makeで生成したチャネルはオープンの状態から始まるが、明示的にクローズすることもできる。クローズには組み込み関数のcloseを使う。クローズされたチャネルに送信するとランタイムパニックを起こす。  
~~~  
ch := make(chan int, 1)
close(ch)
ch <- 1 // ランタイムパニック
~~~  
クローズされたチャネルから受信するのは問題ない、データが空になったりクローズされたりしても、チャネルが内包する型の初期値を送信し続ける。  
~~~  
ch := make(chan int, 3)
ch <- 1
ch <- 2
ch <- 3
close(ch)
<-ch // == 1
<-ch // == 2
<-ch // == 3
<-ch // == 0
<-ch // == 0
~~~  

チャネルがクローズされているかどうかは受信するときに変数を2つ割り当てることで判断できる。  
~~~  
ch := make(chan int)
close(ch)
i, ok := <- ch // i == 0, ok == false
~~~  
第二引数目がfalseを返すのは厳密にいうと**チャネルのバッファ内が空で、かつクローズされた状態**になる。
チャネルのバッファ内にまだ受信可能なデータが残っている場合はokの値はtrueを返す。  

### ゴルーチンとクローズの例
~~~
package main

import(
  "fmt"
  "time"
)

func receive(name string, ch <-chan int) {
  for {
    i, ok := <- ch
    if ok == false {
      break
    }
    fmt.Println(name, i)
  }
  fmt.Println(name + "is done.")
}

func main() {
  ch := make(chan int, 20)

  go receive("1st goroutine", ch)
  go receive("2nd goroutine", ch)
  go receive("3rd goroutine", ch)

  i := 0
  for i < 100 {
    ch <- i
    i ++
  }
  close(ch)

  time.Sleep(3 * time.Second)
}
~~~  
