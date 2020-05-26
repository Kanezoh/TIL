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
