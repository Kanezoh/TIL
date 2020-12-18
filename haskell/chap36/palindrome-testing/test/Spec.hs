import Lib
import Data.Char
import Test.QuickCheck

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = filter (not . isPunctuation) text

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

main :: IO ()
main = do
  putStrLn "Running tests..."

  assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
  assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
  assert (isPalindrome ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  putStrLn "done"
  
