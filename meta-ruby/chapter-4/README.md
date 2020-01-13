# 4章　ブロック

## 4.1 ブロックの基本

基本的なブロックの使用例  

~~~
def a_method(a,b)
  yield(a,b)
end

a_method(1,2){|a,b| a+b} =>3
~~~  

ブロックを定義できるのはメソッドを呼び出すときのみ。メソッド内ではblock_given?を使えばブロックの有無がわかる。  

## 4.3 ブロックはクロージャ

a=2,@b=4 などそのオブジェクトに紐づけられた名前のことを束縛という。ブロックは定義された時にその場の束縛を
一緒に連れていく。  
~~~
def my_method
  x = "Goodbye"
  yield("cruel")
end

x = "Hello"
my_method {|y| "#{x}, #{y} world"}
=> "Hello,cruel world"
~~~

ブロックを作成するとx="Hello"のようなローカルの束縛を包み込む。メソッドにもメソッドの束縛があるが、ブロックは定義された場所の
束縛を見ているため、メソッドの束縛は見えない。  
ブロックの中で新たな束縛を定義することもできるが、ブロックが終了するとともに消えてしまう。  

~~~
def just_yield
  yield
end

top_level_var = 1
just_yield do
  top_level_var += 1
  local_var = 1
end

top_level_var => 2
local_var => Error!
~~~  

上記のような特性があるのでブロックはクロージャだと言われている。クロージャをどう使うかはスコープへの理解が必要だ。  

### スコープ
RubyではKernel#local_variablesで束縛の名前を追跡している。  

~~~
v1 = 1
class MyClass
  v2 = 2
  local_variables => [:v2]
  def my_method
    v3 = 3
    local_variables
  end
  
  local_variables => [:v2]
end

obj = MyClass.new
obj.my_method =>[:v3] #新しいスコープがオープン、スコープの終了と共に消滅
obj.my_method =>[:v3] #さっきのv3とは別物、新しく定義されたv3
local_variables =>[:v1,:obj]
~~~  

### スコープゲート

プログラムがスコープを切り替えて新しいスコープをオープンする場所、３つある。  

- クラス定義
- モジュール定義
- メソッド

### スコープのフラット化

~~~
my_var = "成功"

class MyClass
  # my_varを表示したい
  def my_method
    #my_varを表示したい
  end
end
~~~

Q. スコープゲートを通り抜けて変数を共有するにはどうする？
A. 変数をクロージャに包んで持ち込む  

~~~
MyClass = Class.new do
  puts "#{my_var} =>成功

  define_method :my_method do
    puts "{my_var}" =>成功
  end
end
~~~
スコープゲートをメソッド呼び出しに置き換えて変数を共有するテクニックを**フラットスコープ**という。  

### instance_eval

BasicObject#instance_eval はレシーバのコンテキストでブロックを評価するメソッド。  
~~~
ckass MyClass
  def initialize
    @v = 1
  end
end

obj = MyClass.new

obj.instance_eval do
  self => #<MyClass:~>
  @v => 1
end
~~~  

ブロックの内容はレシーバをselfにしてから評価されるのでprivateメソッドやインスタンス変数にも直接アクセスすることができる。  
同様に束縛も見える。  
このような特徴からinstance_evalに渡したブロックは**コンテキスト探査機**と呼ばれる。  

### クリーンルーム
ブロックを評価するためだけにオブジェクトを生成することがある。  
~~~
class CleanRoom
  def current_temperature
    #~
  end
end

clean_room = CleanRoom.new
clean_room.instance_eval do
  if current_temperature
    #~
  end
end
~~~

使いどころが全く不明だが、どうやらDSLを作成する際に使うらしい（RSpecとかもそうらしい？）　　　

## 4.5 呼び出し可能オブジェクト

ブロックの使用は  
- コードを保管する
- 後で呼び出す  
方式だとわかる、こういう形式をとるのはブロックだけではなく
- Procのなか。(ブロックがオブジェクトになったもの)
- lambdaのなか。(Procの変形)
- メソッドのなか。  

の３種類がある。  

### Proc

ブロックを保管しておいて後で実行するときのために、ブロックをオブジェクト化したProcが必要になる。  
~~~
inc = Proc.new{|x| x + 1}
inc.call(2) => 3
~~~  

lambdaも使える。  
~~~
dec = lambda{|x| x - 1}
dec.class => Proc
dec.call(2) => 1
\# 省略記法もある
p = ->(x){ x + 1}
~~~  

### &修飾

ブロックはメソッドに渡す無名関数のようなものでyieldを使って実行する、しかしそれでは足りないケースがある  
- 他のメソッドにブロックを渡したい
- ブロックをProcに変換したい  

いずれもブロックを指すのに**名前**が必要になるので、引数列の最後に&を付けてブロックを参照できる名前をつける。  

~~~
def math(a,b)
  yield(a,b)
end

def do_math(a,b,&operation)
  math(a,b,&operation)
end

do_math(1,2){|x,y| x * y}
~~~

&は「メソッドに渡されたブロックを受け取ってそれをProcに変換したい」という意味になる。&を付けなければProcのまま。  
~~~
def my_method(&the_proc)
  the_proc
end

p = my_method{|name| "Hello,#{name}"}
p.class => Proc
p.call("Bill") => "Hello,Bill"
~~~  

逆にProcをブロックに戻したいときも&修飾が使える。  
~~~
def my_method(greeting)
  "#{greeting}, #{yield}"
end

my_proc = proc {"Bill}
my_method("Hello,",&my_proc)
~~~

### Proc vs lambda

#### return

Procとlambdaではreturnキーワードの意味が違う。  
~~~
def double(callable_object)
  callable_object.call * 2
end

l = lambda{ return 10 }
double(l) => 20

def another_double
  p = proc.new { return 10 }
  result = p.call
  return result * 2 => ここまで処理されない
end
another_double = 10
~~~  

**明示的なリターンは使わないようにすべし**  

#### 項数

lambdaの方が引数のチェックが厳しい、procは引数が多いと切り落とし、少ないとnilを当てる。  

**結論:lambdaの方が直感的に使えるけど機能は同じだから好みで使おう！**  

### Methodオブジェクト
~~~
class MyClass
  def initialize(value)
    @x = value
  end

  def my_method
    @x
  end
end

obj = MyClass.new(1)
m = obj.method :my_method
m.call => 1
~~~  

Object#methodを使うとメソッドをMethodオブジェクトとして抽出できる。これはMethod#callで呼び出せる。  
メソッドはto_procでProcに変換でき、Procはdefine_methodでメソッドに変換できる。  

### UnboundMethod

UnboundMethod: 元のクラスやモジュールから引き離されたメソッド  
MethodをUnboundMethodにするためにはMethod#unbindを呼び出す。Method#instance_methodを使えば、直接UnboundMethodを呼び出せる。  

~~~
module MyModule
  def my_method
    42
  end
end

unbound = MyModule.instance_method(:my_method)
unbound.class => UnboundMethod
~~~  

UnboundMethodは直接呼び出せないので、bindメソッドを使ってオブジェクトに束縛することで追加できる。  
しかし、元のクラスと同じクラス（またはサブクラス）にしか束縛できない。(Moduleであれば関係ない)  
これが使われるのは特殊なケースのみである。  

## まとめ

- ブロック：定義されたスコープで評価される
- Proc : ブロックと同じ
- lambda : Procとは微妙に挙動が異なる、ブロックやProcと同じくクロージャ
- メソッド：オブジェクトに束縛され、オブジェクトのスコープで評価される
  オブジェクトのスコープから引き剥がして他のオブジェクトに束縛することもできる  


