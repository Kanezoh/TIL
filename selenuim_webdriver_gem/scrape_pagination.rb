require 'nokogiri'
require 'selenium-webdriver'
ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"
#ブラウザ立ち上げモード
#caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--user-agent=#{ua}", 'window-size=1280x800']})
#ヘッドレスモード
caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--headless","--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu", "--user-agent=#{ua}", 'window-size=1280x800']})
driver = Selenium::WebDriver.for :chrome, desired_capabilities: caps
wait = Selenium::WebDriver::Wait.new(:timeout => 60) 

#スクレイピングしたいサイト
driver.navigate.to "https://www.rakuten.co.jp/"
sleep 2

driver.execute_script("document.getElementById('sitem').value='マフラー　夏用　男'")
driver.execute_script("document.getElementById('searchBtn').click()")
sleep 2

scarfs =  driver.find_elements(:css,'div.title h2 a')
scarfs.each do |scarf|
  puts scarf.text
end

loop do
  wait.until {driver.find_element(:class, 'pagination').displayed?}
  has_next_btn = driver.execute_script("return (document.getElementsByClassName('nextPage').length == 1)")

  if has_next_btn 
    sleep 2
    driver.execute_script("document.getElementsByClassName('nextPage')[0].click()")
  else
    driver.save_screenshot("/Users/kanekoshunya/Downloads/last_page2.png")
    break
  end
end