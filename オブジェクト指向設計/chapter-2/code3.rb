class PassChecker
  attr_reader :math,:science,:school

  def initialize(math,science,school=nil)
    @math=math
    @science=science
    @school=school
  end

  def result
    if pass_standard
      'Congratulations! You pass the exam!'
    else
      'Sorry, you are not qualified to study here.'
    end
  end

  def pass_standard
    math + science > 120
  end

  def pass_possibility
    if pass_standard && school.competitive_ratio<1.5
      'high'
    else
      'low'
    end
  end
end

class School
  attr_reader :whole_student,:pass_student

  def initialize(whole_student,pass_student)
    @whole_student = whole_student
    @pass_student = pass_student
  end

  def competitive_ratio
    whole_student / pass_student.to_f
  end
end

# test operation
@school=School.new(230,100)
puts @school.competitive_ratio

puts PassChecker.new(100,50,@school).pass_possibility
