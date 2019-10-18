# 継承によって振る舞いを獲得する

### 6-1 クラスによる継承を理解する
継承とは**メッセージの自動委譲**の仕組み  
あるオブジェクトが受け取ったメッセージに応答できなかったら別のオブジェクトにメッセージを委譲する、という関係  
クラスの場合はサブクラス→スーパークラスへの転送

### 6-2 継承を使うべき箇所を識別する

#### 具象クラスから始める
オブジェクト指向あるある、たい焼きの例を使ってみる
~~~
class Taiyaki
  attr_reader :size,:price

  def initialize(args)
    @size=args[:size]
    @price=args[:price]
  end

  def info
    {
      *デフォルトであんこ
      taste: 'anko'
    }
  end

  〜その他メソッド〜
end
~~~
たい焼きはサイズ、値段の変数をもち、デフォルトとしてあんこ味が設定されている。  
新メニューとしてクリーム味のスペシャルたい焼きを作れるようにしてみる。
~~~
class Taiyaki
  attr_reader :style,:fruit,:size,:price

  def initialize(args)
    @style=args[:style]
    @size=args[:size]
    @fruit=args[:fruit]
    @price=args[:price]
  end

  def info
    if style==:special
      {
        taste: 'cream'
      }
    else
      {
        taste: 'anko'
      }
    end
  end

  〜その他メソッド〜
end
~~~
infoメソッドのstyleによって条件分岐させるところから嫌な匂いがプンプンしてくる。新メニューを増やすたびに条件分岐が増える。  
これは明らかに「誰が」「何をするか」を知っている例であり、リファクタリングの目安になる。前回の同様の例では別のオブジェクトにメッセージを送っていたためダックタイピングを使って解決したが、今回は自身にメッセージを送っている。継承を見抜く目安だ。  
#### 埋め込まれた型を見つける
このTaiyakiクラスは本来別物であるべき型が１つのクラスに同居している例である。styleによって分岐しているが、この名前はtypeだったり、categoryだったりする。こんな名前の変数には注意だ。継承とはこういうタイプが異なるものの、強く関連した型の問題を解決するテクニックである。  

### 6-3 継承を不適切に適用する
ということでさっそく継承を使おう！
~~~
class SpecialTaiyaki < Taiyaki
  attr_reader :fruit
  def initialize(args)
    @fruit=args[:fruit]
    super(args)
  end

  def spares
    super.merge(fruit: fruit)
  end
end

s=SpecialTaiyaki.new(
  size:'small',
  fruit: 'strawberry',
  price: '200'
)

puts s.info => {:taste=>'anko',:fruit=>'strawberry'}⇦あれ、クリーム味じゃなかったっけ？

~~~
何かがおかしい。結局のところ、最初に作ったたい焼きクラスもある程度特化されたものだったのだ。原因はスーパークラスが十分に抽象化されていないことだ、現在のたい焼きクラスは一般的なたい焼きとスペシャルたい焼きの両方の振る舞いを持っている.

### 6-4 抽象を見つける
継承のルール  
- モデル化しているオブジェクトが一般-特殊の関係を持っていること  
- 正しいコーディングテクニックを使っていること  

#### 抽象的なスーパークラスを作る
