class PassChecker
  attr_reader :math,:science

  def initialize(math,science)
    @math=math
    @science=science
  end

  def result
    if (math + science)>120
      puts 'Congratulations! You pass the exam!'
    else
      puts 'Sorry, you are not qualified to study here.'
    end
  end
end

p=PassChecker.new(50,60)
p.result
