# 第３章 依存関係を管理する  

本章では「振る舞いが他のオブジェクトに実装されている時にそれにアクセスする方法」を扱う  

### 3-1 依存関係を理解する

~~~
class PassChecker
  attr_reader :math,:science,whole_student,pass_student

  def initialize(math,science,whole_student,pass_student)
    @math=math
    @science=science
    @whole_student=whole_student
    @pass_student=pass_student
  end
(中略)
  def pass_possibility
    if pass_standard && School.new(whole_student,pass_student).competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end
end

class School
  attr_reader :whole_student,:pass_student

  def initialize(whole_student,pass_student)
    @whole_student = whole_student
    @pass_student = pass_student
  end

  def competitive_ratio
    whole_student / pass_student.to_f
  end
end

PassChecker.new(70,60,230,100).pass_possibility
~~~  

上のコードは前章の最後のコードを少しいじったものです。Schoolへの変更によってPassCheckerへの変更が強制される
状況を書き出してみましょう。  
- 他のクラスの名前：PassCheckerはSchoolという名前のクラスが存在することを予想している
- self以外のどこかに送ろうとするメッセージの名前：PassCheckerはSchoolのインスタンスがcompetitive_ratioに応答することを予想している
- メッセージが要求する引数：PassCheckerはSchool.newにwhole_studentとpass_studentが必要なことを知っている
- それら引数の順番：PassCheckerはSchool.newの最初の引数がwhole_studentで２番目がpass_studentであることを知っている  

オブジェクトは共同作業をする関係上、完全に依存しないことは__不可能__です。しかし、これらの依存関係は必要のないものばかりです。
コード間の結合は出来るだけ疎にしましょう。  

これらの依存関係の解決方法を探る前に...  
依存関係が生み出す他の２つの問題（後の章で詳しく取り上げる）  
- メッセージチェーン(繋いでる間のオブジェクト全てに依存関係が生まれる)
- テストとコードの結合（コードを直すたびにテストが壊れる）

### 3-2 疎結合なコードを書く

#### 依存オブジェクトの注入
~~~
class PassChecker
  attr_reader :math,:science,whole_student,pass_student
(中略)
def pass_possibility
  if pass_standard && School.new(whole_student,pass_student).competitive_ratio<1.5
    'high'
  else
    'low'
  end
end
end
~~~

このコードでPassCheckerクラスではpass_possibilityはSchoolクラスに対して明示的に参照しています  
この参照では、Schoolの名前に変更があった時、pass_possibilityも変更しなければなりません  
しかし、それより大きな問題があります。それは、Schoolクラスがメソッド内という深いところでハードコーディングされているため、
他のオブジェクトとの共同作業を拒絶する点です。例え同じような性質を持ったオブジェクトがあってもこのメソッドは利用できません。  
PassCheckerがアクセスする必要があるのは、Schoolクラスそのものではなく、__competitive_ratioに応答するオブジェクト__です。  
PassCheckerはそのオブジェクトのクラスを気にしたり、知っていたりしてはいけません。  
~~~
class PassChecker
  attr_reader :math,:science,school

  def initialize(math,science,school)
    @math=math
    @science=science
    @school=school
  end
(中略)
  def pass_possibility
    if pass_standard && school.competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end
end

class School
  attr_reader :whole_student,:pass_student

  def initialize(whole_student,pass_student)
    @whole_student = whole_student
    @pass_student = pass_student
  end

  def competitive_ratio
    whole_student / pass_student.to_f
  end
end

PassChecker.new(70,60,School.new(230,100)).pass_possibility
~~~

PassCheckerはこのオブジェクトを@school変数に保持し、schoolメソッドでアクセスするようにしています。  
注意すべきなのは、PassCheckerがcompetitive_ratioに応答するオブジェクトを要求していますが、Schoolクラスを__知らない__ことです。  
今やこのオブジェクトはcompetitive_ratioに応答するオブジェクトとなら共同作業できます。特定のクラスとの依存関係が取り除かれました！  
これは__依存オブジェクトの注入__と呼ばれるテクニックです。  


#### 依存を隔離する  

完璧に依存を取り除くのは不可能だから出来るだけ隔離しましょう！  
もし制約がきつく、SchoolをPassCheckerに注入できない場合は
１、インスタンス生成をPassChecker内のinitializeに任せ、依存を明確にする
~~~
class PassChecker
  attr_reader :math,:science,whole_student,pass_student

  def initialize(math,science,whole_student,pass_student)
    @math=math
    @science=science
    @school=School.new(230,100)
  end
(中略)
  def pass_possibility
    if pass_standard && school.competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end
end

class School
  attr_reader :whole_student,:pass_student

  def initialize(whole_student,pass_student)
    @whole_student = whole_student
    @pass_student = pass_student
  end

  def competitive_ratio
    whole_student / pass_student.to_f
  end
end
~~~

２、Schoolの作成を独自に明示的に定義したメソッド内で行う方法

~~~
class PassChecker
  attr_reader :math,:science,whole_student,pass_student

  def initialize(math,science,whole_student,pass_student)
    @math=math
    @science=science
    @whole_student=whole_student
    @pass_student=pass_student
  end
(中略)
  def pass_possibility
    if pass_standard && school.competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end

  def school
    @school || =School.new(whole_student,pass_student)
  end
end
~~~

これらの解決策では、依然として依存は解決されていません、どちらの例でもSchoolを明示的に生成しています。  
しかし、これにより依存数は減り、依存はより明確になりました。  

#### 脆い外部メッセージは隔離する  
~~~
def pass_possibility
----なんかすごい処理がある---
  if pass_standard && school.competitive_ratio<1.5
----なんかすごい処理がある----
end
~~~

この場合、school.competitive_ratioは奥深くに埋め込まれてしまっています。schoolがcompetitive_ratioに応答する、という
依存はpass_possibilityが負うべきではありません。何かを変更するたびにこのコードを壊す可能性があります。  

~~~
def pass_possibility
----なんかすごい処理がある---
  if pass_standard && competitive_ratio<1.5
----なんかすごい処理がある----
end

def competitive_ratio
  school.competitive_ratio
end
~~~

これはただ単に処理を分けただけに見えますが、外部的な依存を取り除くことに成功しています。
以前の状態ではpass_possibilityはschoolがcompetitive_ratioに応答することを知っていました。
今はこれは独立したメソッドとなり、pass_possibilityはselfに送るメッセージに依存するようになったと言えます。  
また、変更はこのシンプルなラッパーメソッドの範囲に収まります。


### 3-3 依存方向の管理

今までの例ではPassCheckerがSchoolに依存していましたが、これは逆でも書けます。  
依存方向を選択するには次の３つの視点から考えます。  
- あるクラスは他のクラスより変わりやすい
- 具象クラスは抽象クラスよりも変わる可能性が高い
- 多くのところから依存されたクラスを変更すると、広範囲に影響が及ぶ  

2つ目、具象と抽象については3-2の例がわかりやすいです。改善前はSchoolとSchool.newなど極めて具象的なクラスに依存していましたが、依存性を注入してからは__pass_possibilityに応答するクラス__と抽象的なクラスに依存するようになりました。


### まとめ

#### 依存関係を意識して疎結合なコードを書く
#### 依存性が解決できない場合は隔離する
#### クラスの変わりやすさ、具象と抽象などを意識して依存方向を決める
