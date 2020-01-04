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

