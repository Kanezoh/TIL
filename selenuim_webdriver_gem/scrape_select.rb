require 'nokogiri'
require 'selenium-webdriver'
ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"
#ブラウザ立ち上げモード
#caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--user-agent=#{ua}", 'window-size=1280x800']})
#ヘッドレスモード
caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--headless","--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu", "--user-agent=#{ua}", 'window-size=1280x800']})
driver = Selenium::WebDriver.for :chrome, desired_capabilities: caps

#スクレイピングしたいサイト
driver.navigate.to "https://nuro.jp/mansion/service/neworder/"
sleep 1

pref_select = Selenium::WebDriver::Support::Select.new(driver.find_element(:id,'pref-list'))
city_select = Selenium::WebDriver::Support::Select.new(driver.find_element(:id,'city-list'))
town_select = Selenium::WebDriver::Support::Select.new(driver.find_element(:id,'town-list'))


pref_select.select_by(:text,'神奈川県')
sleep 1
city_select.select_by(:text,'横浜市中区')
sleep 1
town_select.select_by(:text,'山手町')
sleep 1
buildings = driver.find_elements(:css,'div#items p span.selectable')
buildings.each do |building|
  puts building.text
end

