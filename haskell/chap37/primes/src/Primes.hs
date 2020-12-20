module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

-- 「エラトステネスのふるい」というアルゴリズム
-- [2,3,4,5]のような並びであれば、まずは2を最初の素数として
-- 他の要素を2で割る、割り切れたら素数ではないので除外
-- これを繰り返す
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= last primes = Nothing
          | otherwise = Just (n `elem` primes)

-- 素因数分解
unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = 
  if n `mod` next == 0
  then next:unsafePrimeFactors (n `div` next) (next:primes)
  else unsafePrimeFactors n primes

-- Maybeで安全にした素因数分解
primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= last primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes
