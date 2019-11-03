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
