class Taiyaki
  attr_reader :size,:price

  def initialize(args)
    @size=args[:size]
    @price=args[:price]
  end

  def info
    {
      taste: 'anko'
    }
  end
end

class SpecialTaiyaki < Taiyaki
  attr_reader :fruit
  def initialize(args)
    @fruit=args[:fruit]
    super(args)
  end

  def info
    super.merge(fruit: fruit)
  end
end

s=SpecialTaiyaki.new(
  size:'small',
  fruit: 'strawberry',
  price: '200'
)

puts s.info
