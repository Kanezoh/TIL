# 継承によって振る舞いを獲得する

### 6-1 クラスによる継承を理解する
継承とは**メッセージの自動委譲**の仕組み  
あるオブジェクトが受け取ったメッセージに応答できなかったら別のオブジェクトにメッセージを委譲する、という関係  
クラスの場合はサブクラス→スーパークラスへの転送

### 6-2 継承を使うべき箇所を識別する

#### 具象クラスから始める
カップ入りのアイスクリームを例にとってみる。
~~~
class IceCream
  attr_reader :size,:cup_color

  def initialize(args)
    @size=args[:size]
    @cup_color=args[:cup_color]
  end

  def info
    {
      flavour: 'vanilla',
      price: 100,
      cup_color: cup_color
    }
  end
end

  〜その他メソッド〜
end
~~~
このクラスはサイズ、カップの色の変数を持ち、味、値段の情報を持つ。  
次に、コーン入りのアイスを作りたい。とりあえずは何も考えずにクラスを拡張してみる。  
~~~
class IceCream
  attr_reader :type,:size,:cup_color,:corn_flavour

  def initialize(args)
    @type=args[:type]
    @size=args[:size]
    @cup_color=args[:cup_color]
    @corn_flavour=args[:corn_flavour]
  end

  def info
    if type==:cup
      {
        flavour: 'vanilla',
        price: 100,
        cup_color: cup_color
      }
    else
      {
        flavour:'vanilla',
        price: 150,
        corn_flavour: corn_flavour
      }
    end
  end
end
~~~
新しくコーンの味の変数、カップかコーンかの分岐のためにtype変数を追加した。  
infoメソッドのtypeによって条件分岐させるところから嫌な匂いがプンプンしてくる。これでは新メニューを増やすたびに条件分岐が増える。  
これは明らかに「誰が」「何をするか」を知っている例であり、リファクタリングの目安になる。前回の同様の例では別のオブジェクトにメッセージを送っていたためダックタイピングを使って解決したが、今回は自身にメッセージを送っている。継承を見抜く目安だ。  
#### 埋め込まれた型を見つける
このIceCreamクラスは本来別物であるべき型が１つのクラスに同居している例である。typeによって分岐しているが、この名前はstyleだったり、categoryだったりする。こんな名前の変数には注意。継承とはこういうタイプが異なるものの、強く関連した型の問題を解決するテクニックである。  

### 6-3 継承を不適切に適用する
ということでさっそく継承を使おう！
~~~
class CornIceCream < IceCream
  attr_reader :corn_flavour
  def initialize(args)
    @corn_flavour=args[:corn_flavour]
    super(args)
  end

  def info
    super.merge(corn_flavour: corn_flavour)
  end
end

corn_ice=CornIceCream.new(
  size: 'S',
  corn_flavour: 'chocolate'
)

puts corn_ice.info
=> {:flavour=>"vanilla", :price=>100, :cup_color=>nil, :corn_flavour=>"chocolate"}
⬆️あれ、コーンアイスを想定してるのにcup_colorも表示されてない？
~~~
結局、最初に作ったアイスクリームクラスもある程度特化されたものだった。スーパークラスは十分に抽象化しなければならない。  
現在のアイスクリームクラスはカップアイスとコーンアイスの両方の振る舞いを持っている.

### 6-4 抽象を見つける
継承のルール  
- モデル化しているオブジェクトが一般-特殊の関係を持っていること  
- 正しいコーディングテクニックを使っていること  

#### 抽象的なスーパークラスを作る
先の例でのIceCreamクラスは実際には特化したものだったので、一般的なクラスに分離してCupIceCreamクラスに分けてみる。  
そして継承元のIceCreamクラス、それを引き継ぐCupIceCream、CornIceCreamという構造にしてみる。  
この場合、IceCreamクラスは抽象クラスであり、このクラスのインスタンスが生成されることは想定していない（実際にjavaでは抽象クラスがnewされることを防ぐためのabstractキーワードがある。Rubyはプログラマを信頼する文化なので存在しない。）  
ちなみに、継承構造を作るにはしっかりと機を見極めなければならない。１つしか作られる見込みのクラスをわざわざスーパークラスとサブクラスに分ける必要はないし、共通のコードを持つクラスがあと何種類登場しそうか、共通のコードを複製するコストはどれほどか。しっかりと考えよう。  
ここではそれらを勘案して十分な理由があるとして話を進める。  
~~~
class IceCream
何もない
end

class CupIceCream < IceCream
かつてのIceCreamクラスのメソッドを移してきた
end
class CornIceCream  < IceCream
コードは前回のまま
end

#### 抽象的な振る舞いを昇格する
size、infoメソッドは全てのアイスに共通する。まずはこれをCupIceCreamクラスから昇格させる。  

~~~
class IceCream
  attr_reader :size

  def initialize(args={})
    @size=args[:size]
  end
end

class CupIceCream < IceCream
  attr_reader :cup_color

  def initialize(args={})
    @cup_color=args[:cup_color]
    super(args)
  end
end

~~~
これで、サブクラスがsizeメソッドを受け取った時、Rubyによって自動でスーパークラスへとメッセージが委譲される。  
ちなみに、継承を設計する際には今回のように一旦全てを具象クラスに下げてから抽象クラスへとあげる方が良い。逆の手順でやると危険な具象メソッドを抽象クラスに残すことになる。  
次にinfoメソッドを見ていく、現在CornIceCreamはinfoメソッドに応答できない（本人も継承元も持っていないため当然）。
しかし、だからと言ってそのまま抽象クラスへと上げるわけにもいかない。
~~~
class CupIceCream < IceCream
  def info
  {
    flavour: 'vanilla',
    price: 100,
    cup_color: cup_color
  }
end

class CornIceCream < IceCream
  def info
    super.merge(corn_flavour: corn_flavour)
  end
end
~~~


CupIceCreamのinfoメソッドは  
- cup_colorは全てのアイスに共通でない  
- price、flavourは全てのアイスに共通だが、同じデフォルト値ではない
という問題を持っている。  
とりあえず、１つ目の問題は置いておき、２つ目について考えてみる。要件は  
- アイスは値段と味を持つ
- 全てのアイスは味について同じ初期値を共有する
- サブクラスは値段について独自の初期値を持つ
- サブクラスの個々のインスタンスは初期値を無視し、インスタンス固有の値を持つことが許される  
これを満たすスーパークラスは
~~~
class IceCream
  attr_reader :size,:price,:flavour

  def initialize(args={})
    @size=args[:size]
    @price=args[:price]
    @flavour=args[:flavour]
  end
end
これで全てのサブクラスがsize,price,flavourに応答できるようになり、固有の値も持てるようになった。
要件の最初と最後は満たしたと言える。
#### テンプレートメソッドパターン

default_priceとdefault_flavourを定義して初期値を表現する。初期値をメソッドでラップすることの利点は、サブクラスがメソッドをオーバーライドすることによって何かに特化できる機会を与えることだ。これをテンプレートメソッドという。
~~~
class IceCream
  attr_reader :size,:price,:flavour

  def initialize(args={})
    @size=args[:size]
    @price=args[:price]|| default_price
    @flavour=args[:flavour] || default_flavour
  end

  def default_flavour
    'vanilla'
  end
end

class CupIceCream < IceCream
  def default_price
    100
  end
end
~~~
### テンプレートメソッドの罠
しかし、ここで終わってはいけない。現在、IceCreamクラスはサブクラスにdefault_priceを実装していることを暗に要求している。コードを一見するだけで把握できない要件を課すことはとんでもない。よって、抽象クラスにもきっちりテンプレートメソッドを実装しよう、こんな具合に。
~~~
class IceCream
  def default_price
    raise NotImplemetedError
      "This #{self.class} cannot respond to"
  end
end
~~~
これで別の人がテンプレートメソッドを実装していない新しいサブクラスを作ってもエラーが出るようになった。  
例えば、テンプレートメソッドのないロールアイスクラスを作ろうとすると
~~~
roll_ice=RollIceCream.new

=> NotImplementedError:
  This RollIceCream cannot respond to:
      'default_price'
~~~

### 6-5 スーパークラスとサブクラスの結合度の管理
十分に抽象化されたスーパークラスができたので、次はinfoメソッドの実装に移る。クラス間の結合度によって実装は変わってくる  

#### 結合度を理解する
infoメソッドのCupIceCreamでの実装はこれ  
~~~
def info
  {
    flavour: 'vanilla',
    price: 100,
    cup_color: cup_color
  }
end
~~~
flavourとpriceは変数に入れられ、IceCreamクラスに昇格している。  
CornIceCreamでの実装はこれ
~~~
def info
  super.merge(corn_flavour: corn_flavour)
end
~~~
このメソッドはスーパークラスにおけるinfoメソッドがハッシュを返すことを予想し、それに要素を追加する。  
つまり、次のinfoメソッドをIceCreamクラスにおけば動作する。  
~~~
class IceCream
  def info
    {
      flavour: 'vanilla',
      price: 100
    }
  end
end
~~~
さて、それではここまで改善したコードを俯瞰してみる  
~~~
class IceCream
  attr_reader :size,:price,:flavour

  def initialize(args={})
    @size=args[:size]
    @price=args[:price]|| default_price
    @flavour=args[:flavour] || default_flavour
  end

  def default_flavour
    'vanilla'
  end

  def info
    {
      flavour: 'vanilla',
      price: 100
    }
  end
end

class CupIceCream < IceCream
  attr_reader :cup_color

  def initialize(args={})
    @cup_color=args[:cup_color]
    super(args)
  end

  def info
    super.merge({cup_color: cup_color})
  end

  def default_price
    100
  end
end

class CornIceCream < IceCream
  attr_reader :corn_flavour

  def initialize(args={})
    @corn_flavour=args[:corn_flavour]
    super(args)
  end

  def info
    super.merge({corn_flavour: corn_flavour})
  end

  def default_price
    150
  end
end
~~~

このコードは十分に動作する。しかし、これらのサブクラスは自身（特化したオプション）について、またスーパークラスに関して（ハッシュを返すinfoを実装していること、initializeに応答すること）を知っている。  
他のクラスについて知識を持つことは必ず依存を生み出す。

#### フックメッセージを使ってコードを疎結合に
前述の依存は、サブクラスがsuperを送る代わりに、スーパークラスが「フック」メッセージを送ることで解決できる。（※フックとは何もしないメソッドの意味）  
百聞は一見に如かずでコードを見てみる。  
~~~
class IceCream
  def initialize(args={})
    @size=args[:size] || default_price
    @price=args[:price]
    @flavour=args[:flavour] || default_flavour

    post_initialize(args)
  end

  def post_initialize
    nil
  end
end

class CupIceCream < IceCream
  def post_initialize(args)
    @cup_color=args[:cup_color]
  end
end
~~~

この変更で  
- superを送らなくて良くなった→アルゴリズムをスーパークラスに移した
- initializeメソッドそのものを取り除けた
また、CupIceCreamは「何を」初期化するかについては知っているが、それが「いつ」行われるかを知らなくて良くなった。  
いつpost_initializeが送られてくるかを知らないし、それがどのオブジェクトから送られてくるかも知らない。  
結合度が格段に低くなった。  
このデザインパターンを適用し、infoメソッドの改善も行える。  
~~~
class IceCream
  def info
    {
      flavour: 'vanilla',
      price: 100
    }.merge(local_options)
   end

   def local_options
      {}
   end
end

class CupIceCream < IceCream
  def local_options
    {cup_color: cup_color}
  end
end

~~~

これで、CupIceCreamはもはやIceCreamがinfoを要求することを知らなくなった。  

### 6-6 まとめ

#### メソッドの抽象性、具象性についてよく考えよう
#### 継承を設計する際は具象クラス→抽象クラスの流れでメソッドを押し上げよう
#### フックメソッドでsuperにより生み出される依存を取り除ける、なるべく結合度を低く保とう
