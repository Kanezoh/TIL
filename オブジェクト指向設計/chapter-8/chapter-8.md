# コンポジションでオブジェクトを組み合わせる

コンポジションとは：組み合わされた全体が単なる部品の集合以上となるように個別の部品を複雑な全体へと組み合わせること

### 8-1 アイスをオプションからコンポーズする
６章で使ったアイスのコードを参考にする。

#### IceCreamクラスを更新する
IceCreamクラスをコンポジションを使うように変更するために、まずいったん現在のコードは忘れる。  
IceCreamクラスにはinfoに答える責任がある。このinfoメソッドはアイスのオプションの一覧を返す。  
つまり、IceCreamクラスがOptionsクラスを持つことは自然に思える。

8-1.png  
~~~
class IceCream
  attr_reader :size,:options

  def initialize(args={})
    @size=args[:size]
    @options=args[:options]
  end

  def info
    options.info
  end
end
~~~

IceCreamクラスの責任は３つになった  
- sizeを知っておくこと
- 自身のoptionsを保持すること
- infoに答えること  

#### Options階層構造を作る
~~~
class Options
  attr_reader :price,:flavour

  def initialize(args={})
    @price=args[:price] || default_price
    @flavour=args[:flavour] || default_flavour

  end

  def info
    {
      price: price,
      flavour: flavour
    }.merge(local_options)
  end

  def default_price
    raise NotImplemetedError
  end

  def local_options
    {}
  end

  def post_initialize
    nil
  end

  def default_flavour
    'vanilla'
  end
end

class CupIceOptions < Options
  attr_reader :cup_color

  def post_initialize(args)
    @cup_color=args[:cup_color]
  end

  def local_options
    {cup_color: cup_color}
  end

  def default_price
    100
  end
end

class CornIceOptions < Options
  attr_reader :corn_flavour

  def post_initialize(args)
    @corn_flavour=args[:corn_flavour]
  end

  def local_options
    {corn_flavour: corn_flavour}
  end

  def default_price
    150
  end
end
~~~

このコードはほぼ完全にIceCreamの階層構造を移したものだ。
このコードによって明白になったのは、そもそも必要だったIceCream特有のコードがいかに少なかったか、である。上のコードのうちほとんどはIceCreamのオプションについて扱うものだった。  

### 8-2 Optionsオブジェクトをコンポーズする
Optionsは個々のオプションについて扱うので、個々のオプションそれぞれについてもクラスを作ることができる。  
OptionsはOptionをたくさん持つことができる、と考える。  
#### Optionを作る
8-2.png

こうしてOptionクラスを新たに導入したことで、Optionsクラスは個々のOptionの配列を包むラッパーとなった。
~~~
class IceCream
  attr_reader :size,:options

  def initialize(args={})
    @size=args[:size]
    @options=args[:options]
  end

  def info
    options.info
  end
end

class Options
  attr_reader :options

  def initialize(options)
    @options=options
  end

  def info
    options.select{|option| option.need_info}
  end
end

class Option
  attr_reader :name,:description,need_info

  def initialize(args)
    @name=args[:name]
    @description=args[:description]
    @need_info=args.fetch(:need_info,true)
  end
end
~~~

と、このようなコードとなり今まで通り動く。しかし、１つ変更点があり、以前はハッシュを返していたinfoメソッドだが、今はOptionオブジェクトの配列を返している。
#### Optionsオブジェクトを配列のように扱う
その役割からしてIceCreamのoptionsメソッドとinfoメソッドは同じ種類のものを返すべきな気がする。しかし、両方にsizeメソッドを送ってみると
~~~
cup_ice.info.size -> 3
cup_ice.options.size -> undefined method 'size' for...
~~~
これは前者がoptionの配列を返しているのに対し、後者はoptionsオブジェクトを返しているからである。
簡単な解決策としてOptionsメソッドに  
~~~
def size
  options.size
end
~~~
を加えればいい。しかし、eachやらsortやら関連のメソッドを付け加えるたびにいちいち追加しなければならないのは面倒だ。  
そこで、こんな解決策もある。  
~~~
class Options < Array
  def info
    select {|option| option.need_info}
  end
end
~~~

しかし、この方法にも問題点がある。Arrayは＋に応答するため、combo_optionを作って２つのoptionを足してみる。  

~~~
combo_option=cup_ice.options + corn_ice.options
combo_option.size -> 7  一見良さそうだが...

'+'が返すオブジェクトはinfoを理解しない
combo_option.info -> NoMethodError ....
~~~

ArrayクラスのメソッドはArrayクラスのインスタンスを返す。Optionsクラスのメソッドに応答しないのでは意味がない。  

よってこんな解決策が最善であるかもしれない。
~~~
require 'forwardable'
class Options
  extend Forwardable
  def delegators :@options,:size,:each
  include Enumerable

  def initialize(options)
    @options=options
  end

  def info
    select{|option| option.need_info}
  end
end
~~~

このOptionsインスタンスに＋メソッドを送ると例外を返す。しかし、Optionsインスタンスはsize,each,Enumerableの全てに対応するようになっている。

#### Optionsを製造する

ここまでコードを改善してきたが、いまだに”何らかのオブジェクトがOptionの作り方を知っていなければならない”という欠点がある。  
特定のアイスクリームのオプションを作ることは簡単である。
~~~
cupice_config=
              [[price: 100],
               [flavour: vanilla],
               [cup_color: red]]
~~~
１列目はオプションの名前、２列目はオプションの値、３列目のneed_infoは真偽値を持ち、オプションの情報を公開するかどうかを決められます。

#### OptionsFactoryを作る

ファクトリー：特定のオブジェクトを製造するオブジェクト  
次のコードはOptionsFactoryモジュールである。
~~~
module OptionsFactory
  def self.build(config,
                 part_class=Part,
                 parts_class=Parts)

  parts_class.new(
    config.collect{|part_config|
      part_class.new(
        name: part_config[0],
        description: part_config[1],
        need_info: part_config.fetch(2,true))})
    end
end
~~~
引数を３つとり、configが１つ、あとはpartとpartsに使われるクラス名である。このファクトリーはconfig配列の構造を知っており、それによる影響は２つ。  
- configをとても短く簡潔に表現できる
- Partsオブジェクトを作るときはこのファクトリーを使うことが当然になること  

~~~
cupice_options=OptionsFactory.build(cupice_config)
~~~
OptionsFactoryは設定用の配列と組み合わされ、有効なOptionsを作るために必要な知識を隔離した。  

#### OptionsFactoryを活用する

Optionクラスに焦点を当てると、わずかにFactoryと重複している。OptionsFactoryが全てのOptionを作るならOptionで必要以上にコードを持つ必要はない。Optionクラス全体は単純なOpenStructで置き換えられる。  
OpenStructクラスはStructクラスと似ている、違いは  
- Structは初期化時に順番を指定して引数を渡す必要がある一方で、OpenStructは初期化時にハッシュをとりそこから属性を引き出す
ことである。Optionクラスを取り除くことでコードが簡潔になり、複雑さが一掃される。以下にコード例を見る。  
~~~
require 'ostruct'
module OptionsFactory
  def self.build(config,
                 parts_class=Parts)

  parts_class.new(
    config.collect{|part_config|
      create_part(part_config)})
  end
  def self.create_part(part_config)
  OpenStruct.new(
    name: part_config[0],
    description: part_config[1],
    need_info: part_config.fetch(2,true))
  end
end
~~~

#### コンポーズされたIceCream

次のコードでコンポジションを使うようになったIceCreamを示す。IceCream、Options、OptionsFactory、設定用の配列から成る。  
IceCreamとOptionsは'has-a'の関係であり、Optionsも'has-a'の関係でOptionオブジェクトの集まりを持つ。  
~~~
class IceCream
  attr_reader :size,:options

  def initialize(args={})
    @size=args[:size]
    @options=args[:options]
  end

  def info
    options.info
  end
end

require 'forwardable'
class Options
  extend Forwardable
  def_delegators :@options,:size,:each
  include Enumerable

  def initialize(options)
    @options=options
  end

  def info
    select {|option| option.need_info}
  end
end

require 'ostruct'
module OptionsFactory
  def self.build(config,parts_class=Parts)
    parts_class.new(
      config.collect{|part_config|
        create_part(part_config)})
  end

  def self.create_part(part_config)
    OpenStruct.new(
          name:part_config[0],
          description: part_config[1],
          need_info: part_config.fetch(2,true))
  end
end

cupice_config=
              [[price: 100],
               [flavour: 'vanilla'],
               [cup_color: 'red']]
cornice_config=
               [[price: 150],
                [flavour: 'vanilla'],
                [corn_flavour: 'choco']]
~~~
### 8-5 コンポジションと継承の選択

継承: is-a関係に使う　デメリット→継承が適さない場合に使うと階層構造による依存が莫大で修正が困難
ダックタイプ： behaves-like関係に使う  
コンポジション： has-a関係に使う  デメリット→全体の見通しがあまり良くないかも、明示的に他のオブジェクトを参照する依存が多い

### 8-6 まとめ
#### コンポジション、クラスによる継承、モジュールを使った振る舞いの共有はそれぞれ違ったコストと利点がある
