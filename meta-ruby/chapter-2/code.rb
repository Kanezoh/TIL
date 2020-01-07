class Array 
  def replace 
    select{|e| !(e.empty?) }
  end
end

arr = ['a','b','']

p arr
p arr.replace
