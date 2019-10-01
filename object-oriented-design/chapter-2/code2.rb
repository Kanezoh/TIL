class PassChecker
  attr_reader :math,:science,:whole_student,:pass_student

  def initialize(math,science,whole_student,pass_student)
    @math=math
    @science=science
    @whole_student=whole_student
    @pass_student=pass_student
  end

  def result
    if (math + science)>120
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def magnification
    whole_student/pass_student.to_f
  end
end

# test operation
p=PassChecker.new(50,60,230,100)
puts p.result
puts p.magnification
