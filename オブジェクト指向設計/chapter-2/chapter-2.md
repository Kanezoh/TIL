# 第２章 単一責任のクラスを設計する

### 2-1 クラスに属するものを決める  
変更が簡単なようにコードを組成する  

「変更が簡単であること」の定義
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
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end
end
~~~

さて、このアプリケーションを公開すると、利用者から「志望校の倍率も表示してほしい」との要望を受けた  
倍率は全体の受験者数 ÷ 合格者数で計算できる  
とりあえずそのまま拡張してみよう
~~~
class PassChecker
  attr_reader :math,:science,:whole_students,:pass_students

  def initialize(math,science,whole_students,pass_students)
    @math=math
    @science=science
    @whole_students=whole_students
    @pass_students=pass_students
  end

  def result
    if (math + science)>120
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def competitive_ratio
    whole_students/pass_students.to_f
  end
end
~~~

”単一責任の原則”に基づいてこのコードを評価してみよう。クラスが単一責任かどうかはそのクラスの持つメソッドを質問に言い換えた
ときに意味のある質問になっているか、で判断できる。  
「PassCheckerさん、あなたのresultは何ですか」これは分かる  
しかし、「PassCheckerさん、あなたのcompetitive_ratio(倍率)は何ですか」これはしっくりこない。  

また、クラスが何をしているか判断するために、クラスを一文で説明する方法がある。説明に「それと」が含まれる場合は２つ以上の責任、
「または」が含まれる場合は全く関係のない２つ以上の責任を持っている可能性が高い。
PassCheckerクラスの責任は何だろう。「受験に関する情報を提供する」だろうか。  
明らかにこれは多くのことをやりすぎている。  
本来必要なのは「点数から合否を判断する」くらいである。  
以上より、現時点でPassCheckerクラスは２つ以上の責任を持っていると言える。  
とはいえ、すぐに新しくクラスを作るよりかは、ひとまず余分な責任を分離しておく方が良い。  
もし今後の変更で、新しいクラスに分類すべき要素が増え、その必要にかられたら作れば良い

#### Struct(構造体)を使って単一責任の原則に基づいて修正
~~~
class PassChecker
  attr_reader :math,:science,:school

  def initialize(math,science,whole_students,pass_students)
    @math=math
    @science=science
    @school=School.new(whole_students,pass_students)
  end

  def result
    if (math + science)>120
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  School = Struct.new(:whole_students,pass_students) do
    def competitive_ratio
      whole_students / pass_students.to_f
    end
  end
end


~~~

これで単一責任の原則は保たれたと言える。

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

このようなラッパーメソッドが実装される。インスタンス変数を使うときはこのメソッドを呼び出せばよく、インスタンス変数に変更がある場合もこのメソッドに変更を加えれば良いだけである。  
例：
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

  def  competitive_ratio
    data.collect {|cell|
      cell[0] / cell[1].to_f
    }
  end
end
~~~

このクラスを初期化するには二次元配列が必要になる
~~~
@data=[[230,100],[150,75]]
~~~

このメソッドを使うにはメッセージの送り手が何のデータが配列のどこにあるのかを把握していなければならない。  
またcompetitive_ratioメソッドも配列のどこに何が入っているか、を把握していなければいけない。
これを解決するには構造体を使い、複雑な構造を隠蔽する必要がある
~~~
class RevealingExample
  attr_reader :numbers

  def initialize(data)
    @numbers=simplify(data)
  end

  def competitive_ratio
    numbers.collect {|number|
      number.whole / number.pass
    }
  end

  Score=Struct.new(:whole,:pass)
  def simplify(data)
    data.collect{|cell|
      Score.new(cell[0],cell[1])
    }
  end
end
~~~
上記のcompetitive_ratioメソッドは配列の内部構造について何も知らず  
- numbersに列挙できる何かがある
- それらの要素がwhole,passに応答する
ことくらいしか知らない。  

#### メソッドを単一責任にする
competitive_ratioメソッドを見ると
~~~
def competitive_ratio
  numbers.collect {|number|
    number.whole / number.pass
  }
end
~~~

２つの責任を持っていることは明白だ。これは
~~~
def competitive_ratio
  numbers.collect{|number| calc_competitive_ratio(number)}
end
def calc_competitive_ratio(number)
  number.whole / number.pass
end
~~~

のように分離できる

同様に、
~~~
def result
  if (math + science)>120
    'Congratulations! You pass the exam!'
  else
    'Sorry, you are not qualified to study here.'
  end
end
~~~
も
~~~
def result
  if pass_standard
    'Congratulations! You pass the exam!'
  else
    'Sorry, you are not qualified to study here.'
  end
end

def pass_standard
  math + science > 120
end
~~~

の方がメソッドの単一責任が保たれる。

### 最終的に

コードのリファクタリングをしていたら利用者が「合格率も表示してくれ」と要望してきた  
新しいクラスを作るには良い知らせだろう  
新しく学校の情報を持つSchoolクラスを作り、そこに倍率の計算を任せた  
また、PassCheckerがSchoolのインスタンスを持てるようにし、そこから合格率を計算できるようにした（こんな式で合格率が表示されたら受験生はたまったものではないが）

~~~
class PassChecker
  attr_reader :math,:science,:school

  def initialize(math,science,school=nil)
    @math=math
    @science=science
    @school=school
  end

  def result
    if pass_standard
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def pass_standard
    math + science > 120
  end

  def pass_possibility
    if pass_standard && school.competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end
end

class School
  attr_reader :whole_students,:pass_students

  def initialize(whole_students,pass_students)
    @whole_students = whole_students
    @pass_students = pass_students
  end

  def competitive_ratio
    whole_students / pass_students.to_f
  end
end
~~~

### まとめ
#### クラス、メソッドについて単一責任の原則を意識する  
#### クラスの振る舞いについて質問したり、クラスを一文で説明しようとすると単一責任かどうかの指標となる  
#### 構造体を使えば、簡易的に単一責任の確保、複雑なデータ構造の隠蔽が行える
