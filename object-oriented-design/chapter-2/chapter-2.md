# 第２章 単一責任のクラスを設計する

### 2-1 クラスに属するものを決める  
変更が簡単なようにコードを組成する  

- 「変更が簡単であること」の定義
  - 変更は副作用をもたらさない
  - 要件の変更が小さければ、コードの変更も小さい
  - 既存のコードは簡単に再利用できる

これらを満たすコードの条件は "TRUE"  
- 見通しが良い(Transparent):変更がもたらす影響が明白
- 合理的(Resonable): 変更にかかるコストが変更がもたらす利益にふさわしい
- 利用性が高い(Usable): 新しい環境、予期していなかった環境でも利用できる
- 模範的(Exemplary): コードに変更を加える人が上記の品質を自然に保つようなコードである  

### 2-2 単一責任を持つクラスを作る

例として予備校のバンザイシステム的なものを作る  
実装はシンプルで、数学、理科の２科目の合計点が120点より上なら合格、それ以外なら不合格を通知する  
さて、この説明から数学、理科の点数を「データ」、合否の判断を「振る舞い」とすると、簡単なPassCheckerクラスを作ることができる
~~~
class PassChecker
  attr_reader :math,:science

  def initialize(math,science)
    @math=math
    @science=science
  end

  def result
    if (math + science)>120
      puts 'Congratulations! You pass the exam!'
    else
      puts 'Sorry, you are not qualified to study here.'
    end
  end
end
~~~

さて、このアプリケーションを公開すると、利用者から「志望校の倍率も表示してほしい」との計算を受けた  
とりあえずそのまま拡張してみよう.
~~~
class PassChecker
  attr_reader :math,:science,:whole_student,:pass_student

  def initialize(math,science,whole_student,pass_student)
    @math=math
    @science=science
    @whole_student=whole_student
    @pass_student=pass_student
  end

  def result
    if (math + science)>120
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def magnification
    whole_student/pass_student.to_f
  end
end
~~~

この時点でcode1のコードはインスタンス生成の際の引数が増えたために動かなくなっている  
とはいえ、変更範囲はその部分だけであり、まだ大きな影響が出ているとは言えない。
しかしそれはあくまでこのアプリケーションの規模が小さいからだ。規模が大きくなると修正箇所も膨大になるため、とても容易に変更可能な
アプリケーションとは言えない。  
それでは、”単一責任の原則”に基づいてこのコードを評価してみよう。クラスが単一責任かどうかはそのクラスの持つメソッドを質問に言い換えた
ときに意味のある質問になっているか、で判断できる。  
「PassCheckerさん、あなたのresultは何ですか」これは分かる  
しかし、「PassCheckerさん、あなたのmagnification(倍率)は何ですか」これはおかしな質問だ。  

また、クラスが何をしているか判断するために、クラスを一文で説明する方法がある。説明に「それと」が含まれる場合は２つ以上の責任、
「または」が含まれる場合は全く関係のない２つ以上の責任を持っている可能性が高い。
PassCheckerクラスの責任は何だろう。「受験に関する情報を提供する」だろうか。  
明らかにこれは多くのことをやりすぎている。  
本来必要なのは「点数から合否を判断する」くらいである。  
以上より、現時点でPassCheckerクラスは２つ以上の責任を持っていると言える。


### 2-3 変更を歓迎するコードを書く

#### データではなく振る舞いに依存する  
~~~

def initialize(math,science)
  @math=math
  @science=science
end

def result
  if (@math + @science)>120
  (以下略)
~~~
上のコードではインスタンス変数をそのまま参照している。これではインスタンス変数に変更があった時に参照されている
全ての箇所を変更しなければならない

~~~
attr_reader :math,:science
def initialize(math,science)
  @math=math
  @science=science
end

def result
  if (math + science)>120
  (以下略)
~~~

こうすると

~~~
def math
  @math
end
~~~

メソッドが実装される。インスタンス変数を使うときはこのメソッドを呼び出せばよく、インスタンス変数に変更がある場合もこの
メソッドに変更を加えれば良いだけである。

~~~
def math
  @math + 10 #簡単に下駄を履かせられる、変更箇所もこの一点のみで済む
end
~~~

#### データ構造の隠蔽
複雑なデータ構造に依存しているコードも良くない

~~~
class ObscuringExample
  attr_reader :data

  def initialize(data)
    @data=data
  end

  def  magnification
    data.collect {|cell|
      cell[0] + (cell[1] * 2)
    }
  end
end
~~~

このクラスを初期化するには二次元配列が必要になる
~~~
@data=[[230,100],[150,75]]
~~~

このメソッドを使うにはメッセージの送り手が何のデータが配列のどこにあるのかを把握していなければならない。  
またmagnificationメソッドも配列のどこに何が入っているか、を把握していなければいけない。
これを解決するには構造体を使い、複雑な構造を隠蔽する必要がある
~~~
class RevealingExample
  attr_reader :numbers

  def initialize(data)
    @numbers=numify(data)
  end

  def magnification
    numbers.collect {|number|
      number.whole / number.pass
    }
  end

  Score=Struct.new(:whole,:pass)
  def numify(data)
    data.collect{|cell|
      Score.new(cell[0],cell[1])
    }
  end
end
~~~
上記のmagnificationメソッドは配列の内部構造について何も知らず  
- numbersに列挙できる何かがある
- それらの要素がwhole,passに応答する
ことくらいしか知らない。  

#### メソッドを単一責任にする
magnificationメソッドを見ると
~~~
def magnification
  numbers.collect {|number|
    number.whole / number.pass
  }
end
~~~

２つの責任を持っていることは明白だ。これは
~~~
def magnification
  numbers.collect{|number| magnificate(number)}
end
def  magnificate(number)
  number.whole / number.pass
end
~~~

のように分離できる
