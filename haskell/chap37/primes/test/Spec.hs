import Test.QuickCheck
import Primes
import Data.Maybe

-- 0未満、最大値より上の入力の時はNothingを返すかどうかをテスト
prop_validPrimesOnly val = if val < 2 || val >= last primes
                           then result == Nothing
                           else isJust result
  where result = isPrime val

-- 素数と判定されたものが本当に素数かをテスト
prop_primesArePrime val = if result == Just True
                          then length divisors == 0
                          else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonpPrimesAreComposite val = if result == Just False
                                  then length divisors > 0
                                  else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonpPrimesAreComposite

