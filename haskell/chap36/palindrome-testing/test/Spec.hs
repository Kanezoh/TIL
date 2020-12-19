import Lib
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Text as T

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_isPalindrome text = isPalindrome text == isPalindrome (T.reverse text)



main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_reverseInvariant
  putStrLn "done"
  
