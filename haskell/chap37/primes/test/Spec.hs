import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly val = if val < 0 || val >= last primes
                           then result == Nothing
                           else isJust result
  where result = isPrime val
main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
