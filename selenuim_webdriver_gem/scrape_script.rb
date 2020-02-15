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

sleep 1
driver.execute_script("document.getElementsByClassName('header__sign-in-link')[0].click()")
sleep 1
driver.execute_script("document.getElementById('user_email').value='asfaltmanta114@gmail.com'")
driver.execute_script("document.getElementById('user_password').value='yukimichikowai'")
sleep 1
driver.execute_script("document.getElementsByClassName('is-button-normal-lg-primary')[0].click()")

doc = Nokogiri::HTML.parse(driver.page_source, nil, 'utf-8')
p doc.at_css('h1.body-header__title').content

driver.save_screenshot('/Users/kanekoshunya/Downloads/skusho.png')
