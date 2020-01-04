class Computer
  def initialize(computer_id,data_source)
    @id = id
    @data_source = data_source
  end

  def mouse
    component :mouse
  end

  def cpu
    component :cpu
  end

  def keyboard
    component :keyboard
  end

  def component(name)
    info = @data_source.send "get_#{name}_info",@id
    price = @data_source.send "get_#{name}_price",@id
    result = "#{name.capitalize}: #{info} ($#{price})"
    return "* #{result}" if price >= 100
    result
  end
end


class Computer
  def initialize(computer_id,data_source)
    @id = id
    @data_source = data_source
  end

  def self.define_component(name)
    define_method(name) do
      info = @data_source.send "get_#{name}_info",@id
      price = @data_source.send "get_#{name}_price",@id
      result = "#{name.capitalize}: #{info} ($#{price})"
      return "* #{result}" if price >= 100
      result
    end
  end
end


class Lawyer
  def method_missing(method,*args)
    puts "called: #{method}(#{args.join(', ')})"
    puts "block was given" if block_given?
  end
end

bob=Lawyer.new
bob.talk_simple("a","b") do
end
