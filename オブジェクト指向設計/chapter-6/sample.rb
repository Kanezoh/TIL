#最初のクラス（カップアイスを想定）
class IceCream
  attr_reader :size,:cup_color

  def initialize(args)
    @size=args[:size]
    @cup_color=args[:cup_color]
  end

  def info
    {
      flavour: 'vanilla',
      price: 100,
      cup_color: cup_color
    }
  end
end
#最初のクラスを拡張（コーンのアイスにも対応できるように）
class IceCream
  attr_reader :type,:size,:cup_color,:corn_flavour

  def initialize(args)
    @type=args[:type]
    @size=args[:size]
    @cup_color=args[:cup_color]
    @corn_flavour=args[:corn_flavour]
  end

  def info
    if type==:cup
      {
        flavour: 'vanilla',
        price: 100,
        cup_color: cup_color
      }
    else
      {
        flavour:'vanilla',
        price: 150,
        corn_flavour: corn_flavour
      }
    end
  end
end

#不適切な継承の利用（最初のIceCreamクラスをそのまま継承）

class CornIceCream < IceCream
  attr_reader :corn_flavour
  def initialize(args)
    @corn_flavour=args[:corn_flavour]
    super(args)
  end

  def info
    super.merge(corn_flavour: corn_flavour)
  end
end

corn_ice=CornIceCream.new(
  size: 'S',
  corn_flavour: 'chocolate'
)

puts corn_ice.info

#最終的な構成
class IceCream
  attr_reader :size,:price,:flavour

  def initialize(args={})
    @size=args[:size]
    @price=args[:price] || default_price
    @flavour=args[:flavour] || default_flavour

    post_initialize(args)
  end

  def post_initialize
    nil
  end

  def info
    {
      flavour: 'vanilla',
      price: 100
    }.merge(local_options)
   end

   def local_options
      {}
   end

   def default_flavour
     'vanilla'
   end

   def default_price
     raise NotImplemetedError
   end

end

class CupIceCream < IceCream
  attr_reader :cup_color

  def post_initialize(args)
    @cup_color=args[:cup_color]
  end

  def local_options
    {cup_color: cup_color}
  end

  def default_price
    100
  end
end

class CornIceCream < IceCream
  attr_reader :corn_flavour

  def post_initialize(args)
    @corn_flavour=args[:corn_flavour]
  end

  def local_options
    {corn_flavour: corn_flavour}
  end

  def default_price
    150
  end
end
