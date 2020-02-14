require 'nokogiri'
require 'selenium-webdriver'
ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"
#ブラウザ立ち上げモード
#caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--user-agent=#{ua}", 'window-size=1280x800']})
#ヘッドレスモード
caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--headless","--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu", "--user-agent=#{ua}", 'window-size=1280x800']})
driver = Selenium::WebDriver.for :chrome, desired_capabilities: caps

#スクレイピングしたいサイト
driver.navigate.to "https://www.rakuten.co.jp/"
sleep 2

search_box = driver.execute_script("document.getElementById('sitem').value='マフラー　夏用　男'")
search_btn = driver.find_element("document.getElementById('sitem').click()")
sleep 2

#scarfs =  driver.find_elements(:css,'div.title h2 a')
#scarfs.each do |scarf|
#  puts scarf.text
#end
#
#max_page = driver.find_elements(:css,'div.dui-pagination a').size
#
#
#max_page.times do |n|
#  if n == max_page - 1
#    driver.save_screenshot("/Users/kanekoshunya/Downloads/last_page.png") and break
#  end
#  to_next_btn = driver.find_element(:class,'nextPage')
#  sleep 2
#  to_next_btn.execute_script("click();")
#end
