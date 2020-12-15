module Main where
import qualified Palindrome

-- main IOアクションでユーザー入力を読み取り、回文判定
main :: IO()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response = if Palindrome.isPalindrome text
                 then "it is!"
                 else "it's not!"
  print response
