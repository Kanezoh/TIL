def monthly_sales
  110
end

target_sales = 100

event "monthly sales is surprisingly high" do
  monthly_sales > target_sales
end

event "monthly sales is low" do
  monthly_sales < target_sales
end
