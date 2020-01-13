# 第3章　メソッド

## 3.1 重複するコード

重複するコード、同じような処理の複数のメソッド定義、どのように解決すればいいだろうか？

## 3.2 動的メソッド

メソッド呼び出しにはドット記法の他にsendメソッドも使える  
~~~
class A
  def my_method
    puts 'A'
  end
end

a = A.new
a.my_method =>'A'
a.send(:my_method) => 'A'
a.send("my_method") => 'A' #文字列でも指定できる  
~~~  

これを使えばコードの実行時に動的に呼び出すメソッドを決められるため、*動的ディスパッチ*と呼ばれる。  
**sendメソッドではprivateメソッドも呼べるので注意！**  

### メソッドを動的に定義  

sendメソッドを使えばメソッドを動的に実行できるが、Module#define_methodを使えば動的に定義もできる。  
~~~
class A
  define_method :my_method do |my_arg|
    my_arg * 3
  end
end

a = A.new
a.my_method(2) =>6
~~~  

この手法は動的メソッドと呼ばれる。  

## 3.3. method_missing

Rubyはメソッド呼び出しの際に継承チェーンからメソッドが見つからなければ、レシーバのmethod_missingメソッドを呼び出す。  

### method_missingのオーバーライド

method_missingは呼び出されたメソッドの名前、その引数、あれば渡されたブロックの情報も持っている。  
~~~
class Lawyer
  def method_missing(method,\*args)
    puts "呼び出した: #{method}(#{args.join(', ')})"
    puts "(ブロックも渡した)" if block_given?
  end
end

bob = Lawyer.new
bob.talk_simple('a','b') do
  #ブロック
end

=>呼び出した：talk_simple(a,b)
  (ブロックも渡した)
~~~  

### ゴーストメソッド

同じようなメソッドを定義するときはmethod_missingの呼び出しで適切な処理を行わせることもできる。  
実際にはメソッドが定義されていないのに表面的には呼び出しにきちんと応じているように見えるため、これを**ゴーストメソッド**と呼ぶ。  
### respond_to_missing?

インスタンスが特定のメソッドに反応するかどうかはrespond_to?メソッドでわかる。しかし、ゴーストメソッドは実際には存在しないメソッドであるため、respond_to?では判断できない。  
respond_to?は内部で、メソッドがmethod_missingで反応された時にtrueを返すrespond_to_missingメソッドが存在する。
method_missingをオーバーライドしたらこのメソッドもオーバーライドしてrespond_to?で正しい結果を返すべき。  

~~~
class Sample
  def method_missing(name, *args)
    if name =~ /^to_*/
      [name, *args] # => [:to_sample, "sample args1", "sample args2"]
      return
    else
      super
    end
  end

  def respond_to_missing?(sym, include_private)
    (sym =~ /^to_*/) ? true : super
  end
end
~~~  

### const_missing

似たようなメソッドでModule#const_missingというものがある。存在しない定数が参照されるとRubyは定数の名前をconst_missingにシンボルとして渡す。Rakeでは古い定数名を廃止する際にこのメソッドをオーバーライドして、呼び出された場合に警告を出すようにした。  
  
## 3.4 ゴーストメソッドのバグ

ゴーストメソッドのバグには気づきにくい。  
~~~
class Roulette
  def method_missing(name,*args)
    person = name.to_s.capitalize
    3.times do
      number = rand(10) + 1
      puts "#{number}
    end
    "#{person} got a #{number}"
  end
end
~~~  

