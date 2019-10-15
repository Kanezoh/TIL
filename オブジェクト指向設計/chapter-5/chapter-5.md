# 第５章 ダックタイピングでコストを削減する

### ダックタイプとは
どの特定のクラスとも結びつかないパブリックインターフェース、オブジェクト同士の結合を疎に保ち、柔軟なアプリケーションを構築するのに非常に重要  

#### ダックを見逃す
~~~
class Staff
   attr_reader :items

   def prepare(repairman)
      repairman.prepare_items(items)
   end
   (略)
end
class Repairman
  def prepare_items(items)
    items.each {|item| prepare_item(item)}
  end

  def prepare_item(item)
    (略)
  end
end
~~~
対応するシーケンス図はこちら
![図1]()

prepareメソッドはRepairmanに明確な依存はしていないが、prepare_itemsに応答するオブジェクトを受け取ることには依存している。
次に、要件が変わって準備に関わるクラスが増加したことを想定してみる。CheckmanとPackingmanを追加する。

~~~
class Staff
  :items

  def prepare(preparers)
    preparers.each {|preparer|
      case preparer
      when Repairman
        preparer.prepare_items(items)
      when Checkman
        preparer.check_items(items)
      when Packingman
        preparer.pack_items(item)
      end
    }
  end
end

class Checkman
  def check_items(items)
  (略)
  end
end

class Packingman
  def pack_items(items)
  (略)
  end
end

~~~

こうなると、prepareのリスクが異様に高まっていることが分かる。各クラスを名前で参照している上にそれぞれのメソッドの名前も知っている。つまり、他のメソッドからは使えないし、新しくこのメソッドを他のオブジェクトから使うには複雑な分岐を追加しなければならない。悪いコードは自己増殖する、とはこう言うことらしい。

#### ダックを見つける

依存を取り除くためには「Staffのprepareメソッドは単一の目的を果たすためにあるので、その引数も同じ目的を共に達成するために送られてくる」と言うことである。prepareの目的はスタッフが客に渡すアイテムの準備をすること、にある。つまり、メッセージを送る先はpreparer(準備する人)と抽象化できる。個々のオブジェクトではなく、prepare_itemsに対応する複数のpreparerを想定すると、より柔軟な設計となる。
図２
Preparerは具象ではなく、抽象的なインターフェースであり、prepare_itemsを実装するPreparerである。つまり、Repairman、そして先の例で追加したCheckman,PackingmanがPreparerになれば良い。

~~~
class Staff
  attr_reader :items

  def prepare(preparers)
    preparers.each {|preparer| preparer.prepare_items(self)}
  end
end

class Repairman
  def prepare_items(staff)
    staff.items.each {|hoge| hogehoge}
  end
end

(以下略)
~~~

#### ダックタイピングの影響

具象的なコード：理解はしやすいが拡張に危険が伴う  
抽象的なコード：最初の理解は難しくなるが、拡張は容易になる

### ダックを信頼するコードを書く

#### 隠れたダックを認識する
ダックの存在を示唆するコード  
- クラスで分岐するcase文
- kind_of?とis_a?
- responds_to?

#### ダックタイプを文書化する
ダックタイプは抽象的であり、コードとしては仮想的な部分なので文書化、テストを行わなければならない。
#### ダック間でコードを共有する
今回の例で扱ったRepairman,Checkmanなどのクラスはインターフェースのみを共有していたが、ダックタイプではその他の振る舞いもいくらか共有することが多い。７章で扱う。
#### 賢くダックを選ぶ
kind_of?などで条件を分岐させるようなコードはあまり良くないと学んだ。しかし、Rubyの基本クラス(Integer,Hashなど)など安定したクラスで分岐する場合は、無理にダックにする必要はない。モンキーパッチを利用することで基本クラスにも変更を加えることはできるが、まあわざわざやるまでもない。何でもかんでもダックにすればいいわけではないので適宜、クラスの変更可能性などを考慮しながら導入すべきである。  

### ダックタイピングへの恐れを克服する
静的型付け言語からの移住者に向けた動的、静的言語の長短の比較  

### まとめ
#### ダックタイピングはインターフェースを特定の型から切り離し、「何であるか」ではなく「何をするか」によって定義される仮想の型を作る
#### ダックタイピングは根底にある抽象を明らかにし、コードのリスクを低減し、柔軟性を高める
