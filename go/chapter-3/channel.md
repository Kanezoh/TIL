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

## チャネルとfor

チャネルに対しても「範囲節によるfor」を適用できる、チャネルからひたすら受信し続ける時に有用。  
~~~
ch := make(chan int 3)
ch <- 1
ch <- 2
ch <- 3
for i := range ch {
  fmt.Println(i)
}
# =>
1
2
3
~~~  

## select
~~~
e1 := <- ch1
e2 := < -ch2
~~~  
ch1, ch2はチャネル型、e1,e2はチャネルを内包するデータ型である単純な例を考える。  
ch1チャネルからデータが受信できない場合、この処理の流れを実行するゴルーチンは停止し、いつまでも次の処理にたどり着けない。  
Goにはこのような問題を解決するために、ゴルーチンを停止させずに複数のチャネルを操作する構文が用意されている。  
~~~
select {
  case e1 := <- ch1
    // ch1で処理が成功した場合の処理
  case e2 := <- ch2
    // ch2で処理が成功した場合の処理
  default:
    // その他の処理
}
~~~  
select文のcase式は全てチャネル操作を伴っている必要がある。  
~~~
// ch1から受信
case e1 := < -ch1
// ch2から受信(2変数)
case e2, ok := <-ch2
//ch3へe3を送信
case ch3 <- e3
// ch5から受信したデータをch4へ送信
case ch4 <- (<-ch5)
~~~  

### チャネルとselectを利用したサンプルコード

~~~  
ch1 := make(chan int)
ch2 := make(chan int)
ch3 := make(chan int)

// ch1から受信した整数を2倍してch2へ送信
go func() {
  for{
    i := <-ch1
    ch2 <- (i * 2)
  }
}()
// ch2から受信した整数を1減算してch3へ送信
go func() {
  for {
    i := <- ch2
    ch3 <- (i - 1)
  }
}()
n := 1
LOOP:
for {
  select {
    case ch1 <- n:
      n++
    case i := <-ch3
      fmt.Println("received", i)
    default:
      if n > 100 {
        break LOOP
      }
  }
}
// 出力結果
received 1
received 3
received 5
received 7
received 9
received 11
received 13
received 15
~~~  
このように、select文を使うことで非同期で行われるゴルーチンの処理を適切に制御できる。
