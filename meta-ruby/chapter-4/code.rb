def my_method(a)
  "#{a},#{yield}"
end

my_proc = proc {"Bill"}
p my_method("Hello",&my_proc)
