module Lib
    ( isPalindrome
    ) where

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = filter (not . (== '!')) text
