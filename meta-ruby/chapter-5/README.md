# メタプログラミングRuby 第５章　クラス定義

## 5.1 クラス定義の分かりやすい説明

### 5.1.1 クラス定義の中身
クラス定義はメソッドを定義するだけの場所ではなく、あらゆるコードを書く事ができる。  

~~~
class MyClass
  puts "Hello"
end
=> Hello

result = class MyClass
  puts self
end
=> MyClass

~~~  

### 5.1.2 カレントクラス

Rubyのプログラムは常にカレントオブジェクトselfを持っている。  
同様に、カレントクラス（あるいはカレントモジュール）も持っており、メソッドを定義するとカレントクラスの
インスタンスメソッドになる。  
カレントクラスはカレントオブジェクトのselfのようにキーワードで参照はできないが目視で確認できる。  

- プログラムのトップレベルではカレントクラスはmainのクラスのObjectになる。 
- classキーワードでクラスをオープンするとそのクラスがカレントクラスになる（モジュールの場合でも同様）  
- メソッドの中ではカレントオブジェクトのクラスがカレントクラスになる。  

例  

~~~
class C
  def m1
    p self
    def m2; end
  end
end

class D < C; end

obj = D.new

obj.m1 => D

C.instance_methods(false) => [:m1,:m2]
D.instance_methods(false) => []
~~~
カレントオブジェクトはDクラスのインスタンスだが、カレントクラスはCクラスとなっている。よって、m２もCクラスのインスタンス
メソッドとなっている。  


先で挙げた真ん中のケースの通り、classキーワードでクラスをオープンする事でカレントクラスを変更する事ができるが、
クラスの名前がわからないとclassキーワードは使えない。そこで、クラス名がわからなくてもクラスをオープンする方法がある。 

*class_eval*メソッドだ

Module#class_evalはそこにあるクラスのコンテキストでブロックを評価する。  

~~~
def add_method_to(a_class)
  a_class.class_eval do
    def m; "HELLO" end
  end
end

add_method_to String
"ABC".m => HELLO
~~~  

class_evalではinstance_evalとは違う。後者はselfを変更するのみだが、前者はカレントクラスもselfも変更する。  
カレントクラスの変更により、classキーワードと同じようにクラスをオープンする事ができる。  
また、class_evalはclassキーワードより柔軟であり、classが定数を必要とするのに対して、クラスを参照する変数ならなんでも良く、
classキーワードが現在の束縛を捨てて新しいスコープを作る一方でclass_evalはフラットスコープを持っているなどの特徴がある。  

### カレントクラスのまとめ

- Rubyのインタプリタは常にカレントクラスの参照を追跡している。  
- クラス定義の中では、カレントオブジェクトselfとカレントクラスは一致している。
- クラスへの参照を持っていれば、クラスはclass_evalでオープンできる。

### 5.1.3 クラスインスタンス変数

クラスのインスタンス変数とクラスのオブジェクトのインスタンス変数は全くの別物である。  

~~~
class A
  @my_var = 1
  def self.read; @my_var; end
  def write; @my_var = 2; end
  def read; @my_var; end

end

obj = A.new
obj.read => nil
obj.write
obj.read => 2
A.read => 1
~~~ 

上記のコードではそれぞれスコープも属しているオブジェクトも違う変数が同じ名前で定義されている。  
２つ目のmy_varはobjがselfとなる場所に定義されているobjオブジェクトのインスタンス変数である。  
１つ目のmy_varはAがselfとなる場所に定義されているAというオブジェクトのインスタンス変数であり、これは_クラスインスタンス変数_と呼ばれる。  


## 5.2 クラスのタブー

クイズ：classキーワードを使わずに以下のコードを書くには？  
~~~
class MyClass < Array
  def my_method
    'Hello!'
  end
end
~~~  

### 5.2.1 答え

~~~
c = Class.new(Array) do
  def my_method
    'Hello!'
  end
end

~~~

これでクラスを参照する事ができた。しかし、まだこのクラスは無名である。以前学んだように、クラス名はただの定数であるから
自分で割り当てる事ができる。
~~~
MyClass = c
~~~  

## 5.3 特異メソッド

リファクタリング中にこんなコードを見つけた。  
~~~
class Paragraph
  def initialize(text)
    @text = text
  end

  def title?; @text.upcase = @text; end
  def reverse; @text.reverse; end
  def upcase; @text.upcase; end
  ...
  ...
  ...
end
~~~  

title?以外のメソッドは文字列に処理を委譲している。では、title?がどこで使われてるかというと  
~~~
def index(paragraph)
  add_to_index(paragraph) if paragraph.title?
end
~~~  

どうせほとんど文字列に処理を委譲しているからStringクラスにモンキーパッチでtitle?を当てればいいのではないか？  
しかし、title?メソッドは文字列が段落の時以外には使えない。どうしよう？  

### 5.3.1 特異メソッドの導入

~~~
str = "just a regular string" #普通の文字列

def str.title?
  self.upcase == self
end

str.title? => false
str.methods.grep(/title?/) => [:title?]
str.singleton_methods => [:title?]
~~~

上記のコードではstrオブジェクトのみにメソッドを追加しており、他のオブジェクトに影響はない。
このように単一のオブジェクトのみに特化したメソッドのことを*特異メソッド*と呼ぶ。

解決策  
~~~
paragraph = "ababa"

def paragraph.title?
  self.upcase == self
end

index(paragraph)
~~~

### 5.3.2 クラスメソッドの真実

クラスは単なるオブジェクトであり、クラス名は単なる定数、ということを思い出すとあることに気づく。  

~~~
obj.a_method =>変数で参照したオブジェクトのメソッドを呼び出す
AClass.class_method => 定数で参照したオブジェクト（つまりクラス）のメソッドを呼び出す
~~~  

つまり、クラスメソッドはクラスというオブジェクトの特異メソッドということである。

### 5.3.3 クラスマクロ

Rubyのオブジェクトにはアトリビュートがない。アトリビュートを実装するにはミミックメソッド（言語要素に擬態したメソッド）を使用する。  

~~~
class MyClass
  def my_attribute=(value)
    @my_attribute = value
  end

  def my_attribute
    @my_attribute
  end
end

obj = MyClass.new
obj.my_attribute = 'ababa'
obj.my_attribute => 'ababa'

~~~

このようなメソッドはアクセサとも呼ばれる。定義するのが面倒なのでModele#attr 系のメソッドを使えば楽に定義できる。  
~~~
class MyClass
  attr_accessor :my_attribute
end
~~~

これらの便利メソッドはModuleクラスで定義されているのでselfがモジュールでもクラスでも使える。このようなメソッドを*クラスマクロ*
と呼ぶ。キーワードっぽく見えるが、クラス定義の中で使える便利なメソッドである。

#### クラスマクロの適用

リファクタリングしているコードのBookクラスにGetTitle,title2,LEND_TO_USERのようなメソッドがある。Rubyの規約ではそれぞれ
title,subtitle,lend_toにすべきだ。しかし、Bookクラスを使っているプロジェクトは他にもあるため、単にメソッドをリネームして
は不具合が生じる。ここでクラスマクロの出番だ。  

~~~ 
class Book
  def title
    ...
  end

  def subtitle ...

  def lend_to(user)
    ...
  end

  def self.deprecate(old_method,new_method)
    define_method(old_method) do |*args,&block| =>古いメソッドをオーバーライド、警告を出して新しいメソッドで処理
      warn "Warning: #{old_method}() is deprecated. Use #{new_method}()."
      send(new_method,*args,&block)
    end
  end

  deprecate :GetTitle, :title
  deprecate :title2, :subtitle
  deprecate :LEND_TO_USER, :lend_to
end

~~~

これでメソッドの処理はそのままに警告を出して新しいメソッドへの移行を促す事ができた。  

## 5.4 特異クラス

### 5.4.1 特異メソッドの謎

以前のメソッド探索の章でメソッドはクラスに住んでいることを学んだ。オブジェクトはメソッドが呼び出されたらclassに向かって右に、
そこからsuperclassを探索するために上に進んでいく。では、特異メソッドはどこにいるのだろうか？  
classに住むことはできない、classに住んでいたらクラスから生成されるインスタンス全てのメソッドになってしまう。
クラスメソッドについても同様にどこにあるのだろうか？  

### 5.4.2 特異クラスの出現

実は、オブジェクトは通常のクラスだけでなく裏に特別なクラスを有している。それが*特異クラス*と呼ばれるものだ。
通常は見えないが、classキーワードを使った特別な構文を使えば正体をつかむ事ができる。  

~~~
class << an_object
  #ここがan_objectのクラス領域
end
~~~  
例えば、特異クラスの参照を取得するにはselfを外に返せば良い  

~~~
obj = Object.new

singleton_class = class << obj
  self
end

singleton_class.class => Class 
~~~

他にもObject#singleton_class を使っても特異クラスの存在を確認できる。  
~~~
"abc".singleton_class => Class  
~~~  

これらの例から特異クラスは確かにクラスである事がわかる。しかし、特別なクラスであり、インスタンスは１つしか持てないし、
継承もできない。そして重要なのは *特異クラスはオブジェクトの特異メソッドが住んでいる場所だ* という事だ。  

~~~
def obj.my_singleton_method; end
singleton_class.instance_methods.grep(/my\_/) =>[:my_singleton_method]
~~~

