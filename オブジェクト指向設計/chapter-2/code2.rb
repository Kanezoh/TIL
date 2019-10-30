class PassChecker
  attr_reader :math,:science,:whole_students,:pass_students

  def initialize(math,science,whole_students,pass_students)
    @math=math
    @science=science
    @whole_students=whole_students
    @pass_students=pass_students
  end

  def result
    if (math + science)>120
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def competitive_ratio
    whole_students/pass_students.to_f
  end
end

# test operation
p=PassChecker.new(50,60,230,100)
puts p.result
puts p.competitive_ratio
