# 6章　コードを記述するコード

## 6.1 attr_checked

attr_accessorに似たクラスマクロ、attr_checkedを作ってみる。  
このメソッドでは、アトリビュートの名前とブロックを受け取り、ブロックで妥当性を評価する。  
動作例  
~~~ 
class Person
  include CheckedAttributes

  attr_checked :age do |v|
    v >= 18
  end
end

me = Person.new
me.age = 39 => OK
me.age = 15 => NG!
~~~  

## 6.2 Kernel#eval

Kernel#evalはRubyのコードの文字列を受け取り、そのコードを実行する。  
~~~
array = [10,20]
element = 30
eval("array << element") => [10,20,30]
~~~  

### REST Clientの例

RESST Client gemではget,post,put,deleteメソッドをそれぞれ定義しているが、これらをevalを使って一気に定義している。  
~~~
POSSIBLE_VERBS = ['get','put','post','delete]

POSSIBLE_VERBS.each do |m|
  eval <<-end_eval
    def #{m}(path,*args,&b)
      r[path].#{m}(*args,&b)
    end
  end_eval
end
~~~  

Kernel#evalを使いこなすにはBindingオブジェクトの理解も必要  

### Binding オブジェクト

Binding:スコープをオブジェクトにまとめたもの。ローカルスコープを取得すれば、そのスコープを持ち回すことができる。  
~~~
class MyClass
  def my_method
    @x = 1
    binding
  end
end

b = MyClass.new.my_method

eval "@x",b => 1
~~~  

また、デフォルトでトップレベルのスコープのBindingがTOPLEVEL_BINDINGという定数で参照できる。  
~~~
class AnotherClass
  def my_method
    eval "self",TOPLEVEL_BINDING
  end
end

AnotheClass.new.my_mehotd => main
~~~  

Pryで使われているbinding.pryなども「現在のスコープでpryを開く」という意味になる。  

### ちなみに
同じようなメソッドのinstance_eval,class_evalはブロックを受け取ったが、実はKernel#evalのように文字列も受け取れる。 

### evalの問題点

- エディタの補完機能が使えない＝構文エラーがわからない
- 読むのも修正するのも難しい  
- コードインジェクション

#### 代替案
動的メソッド、動的ディスパッチを使ってもだいたい同じようなことができる。  
~~~
POSSIBLE_VERBS.each do |m|
  define_method m do |path,*args,&b|
    r[path].send(m,*args,&b)
  end
end
~~~
  
### オブジェクトの汚染
Rubyは安全ではないオブジェクト(ウェブフォーム、ファイル、コマンドライン、システム変数など外部のオブジェクト)に汚染の証をつける  
~~~
user_input = "User input: #{gets()}"
puts user_input.tainted? => true
~~~  

### attr_checked 手順１
~~~
def add_checked_attribute(klass,attribute)
  eval " 
    class klass 
      def attribute
        @{attribute}
      end

      def attribute=(value)
        @#{attribute} = value
      end
    end
  "
end
~~~  

### 6.4 attr_checked 手順２

evalは危険なので早速evalを削除しよう。  

~~~
def add_checked_attribute(klass,attribute)
  klass.class_eval do {
    define_method attribute do
      instance_variable_get @#{attribute}
    end

    define_method "#{attribute=}" do |value,&validation| {
      instance_variable_set("@#{attribute}",value)
    end
  end
end
~~~  

### 6.5 attr_checked 手順３  

バリデーションを追加
~~~
def add_checked_attribute(klass,attribute)
  klass.class_eval do {
    define_method attribute do
      instance_variable_get @#{attribute}
    end

    define_method "#{attribute=}" do |value,&validation| {
      raise "Invalid Attribute" unless validation.call #ブロックを評価してバリデーション
      instance_variable_set("@#{attribute}",value)
    end
  end
end
~~~  

### 6.6 attr_checked 手順4
Classクラスのインスタンスメソッドにしてクラスマクロにする。

~~~
class Class
  def add_checked_attribute(klass,attribute)
    klass.class_eval do {
      define_method attribute do
        instance_variable_get @#{attribute}
      end

      define_method "#{attribute=}" do |value,&validation| {
        raise "Invalid Attribute" unless validation.call #ブロックを評価してバリデーション
        instance_variable_set("@#{attribute}",value)
      end
    end
  end
end
~~~  

### 6.7 フックメソッド

Rubyにはオブジェクトモデルで発生するイベントをキャッチするためのメソッドがある。例えば継承をキャッチするinheritedメソッドがある。  
~~~
class String
  def self.inherited(subclass)
    puts "#{self} is inherited by #{subclass}"
  end
end

class MyString < String; end => " String is inherited by MyString"
~~~  

これらは*フックメソッド*と呼ばれるデフォルトでは何もしないメソッドである。他にもincluded,prepended,extendedなど色々。  

#### フックを使ったイディオム

VCRというgemではモジュールをincludeした際に、そのモジュールのメソッドをインスタンスメソッドではなく、クラスメソッドとして
追加する仕組みを、イディオムを使って実装している。  
~~~
module VCR
  module Normalizers
    module Body
      def self.included(klass)
        klass.extend ClassMethods
      end

      module ClassMethods
        〜クラスメソッド一覧〜
~~~  

この処理の流れは
1. Bodyモジュールをインクルード
2. Bodyモジュールのフックメソッドincludedが呼ばれ、インクルードを要求したクラスにClassMethodsモジュールがエクステンド
3. extendメソッドがClassMethodsのメソッドを特異クラスとして追加



