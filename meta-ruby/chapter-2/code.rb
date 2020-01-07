class Array 
  def replace 
    select{|e| !(e.empty?) }
  end
end

arr = ['a','b','']

p arr
p arr.replace

#######################3



Ababa = ::Array
arr = Ababa.new(4,'aba')
p arr
