require 'nokogiri'
require 'selenium-webdriver'
require 'date'
ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"
#ブラウザ立ち上げモード
#caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--user-agent=#{ua}", 'window-size=1280x800']})
#ヘッドレスモード
caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--headless","--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu", "--user-agent=#{ua}", 'window-size=1280x800']})
driver = Selenium::WebDriver.for :chrome, desired_capabilities: caps

#スクレイピングしたいサイト
sleep 2
driver.navigate.to "https://nyusatsu-joho.e-kanagawa.lg.jp/DENTYO/GPPI_MENU"
sleep 2
driver.execute_script("document.querySelector('table tr:nth-child(3) td:nth-child(2) a').click()")
iframe = driver.find_element(:css,'html frameset frame')
driver.switch_to.frame(iframe)
sleep 2
driver.execute_script("document.querySelector('table.MENU_JUCHU tr:nth-child(4) td:nth-child(2) a').click()")
today = Date.today

from_year = Selenium::WebDriver::Support::Select.new(driver.find_element(:name,'ddl_kokokuYearStart'))
from_year.select_by(:value,"#{today.year - 1}")
from_month = driver.find_element(:name,'txt_kokokuMonthStart')
from_month.send_keys "#{today.month}"
from_day = driver.find_element(:name,'txt_kokokuDayStart')
from_day.send_keys "#{today.day}"

til_year = Selenium::WebDriver::Support::Select.new(driver.find_element(:name,'ddl_kokokuYearEnd'))
til_year.select_by(:value,"#{today.year}")
til_month = driver.find_element(:name,'txt_kokokuMonthEnd')
til_month.send_keys "#{today.month}"
til_day = driver.find_element(:name,'txt_kokokuDayEnd')
til_day.send_keys "#{today.day}"

display_num = Selenium::WebDriver::Support::Select.new(driver.find_element(:name,'ddl_pageSize'))
display_num.select_by(:value,"100")

submit_btn = driver.find_element(:name,'data1')
sleep 2
submit_btn.click

# 「次へ」ボタンがある時のみループ
loop do
  # 詳細ボタンクリックループ
  3.step do |i|
    detail_btn = driver.execute_script('return !!(document.querySelector("form table:nth-of-type(3) tr:nth-child('"#{i}"') td:nth-of-type(1) input[type=\'button\']"))')

    if detail_btn
      sleep 2
      driver.find_element(:css,"form table:nth-of-type(3) tr:nth-child(#{i}) td:nth-of-type(1) input[type='button']").click
      driver.switch_to.window(driver.window_handles[1])
      sleep 1
      driver.close
      driver.switch_to.window(driver.window_handles[0])
    else
      puts '最後の要素です'
      break
    end
  end

  next_btn = driver.execute_script('return !!(document.querySelector("form table:nth-of-type(1) tr td:nth-child(2) a:last-child"))')
  if next_btn
    sleep 2
    driver.execute_script('document.querySelector("form table:nth-of-type(1) tr td:nth-child(2) a:last-child").click()')
  else
    break
  end
end

