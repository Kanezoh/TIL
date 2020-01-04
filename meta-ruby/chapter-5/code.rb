class C
  def m1
    puts self
    def m2
      puts self
    end
  end
end

class D < C; end

obj = D.new
obj.m1

puts C.instance_methods(false)
