require 'nokogiri'
require 'selenium-webdriver'
ua = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"
#ブラウザ立ち上げモード
#caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--user-agent=#{ua}", 'window-size=1280x800']})
#ヘッドレスモード
caps = Selenium::WebDriver::Remote::Capabilities.chrome('chromeOptions' => {args: ["--headless","--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu", "--user-agent=#{ua}", 'window-size=1280x800']})
driver = Selenium::WebDriver.for :chrome, desired_capabilities: caps

#スクレイピングしたいサイト
driver.navigate.to "http://kowabana.jp"
sleep 1


login_button = driver.execute_script("document.getElementsByClassName('header__sign-in-link')")
p login_button
#sleep 1
#login_button.click
#
#input_email = driver.find_element(:id,'user_email')
#input_password = driver.find_element(:id,'user_password')
#submit_button = driver.find_element(:class,'is-button-normal-lg-primary')
#
#input_email.send_keys 'asfaltmanta114@gmail.com'
#input_password.send_keys 'yukimichikowai'
#sleep 1
#submit_button.click
#
#doc = Nokogiri::HTML.parse(driver.page_source, nil, 'utf-8')
#p doc.at_css('h1.body-header__title').content

#driver.save_screenshot('/Users/kanekoshunya/Downloads/skusho.png')
