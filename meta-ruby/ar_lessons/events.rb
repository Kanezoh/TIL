setup do
  puts "set up height of sky"
  @sky_height = 100
end

setup do
  puts "set up height of Mt"
  @mountains_height = 200
end

event "sky drop" do
  @sky_height < 300
end

event "sky approach" do
  @sky_height < @mountain_height
end

event "world end" do
  @sky_height < 0
end


