module Primes where

primes :: [Int]
primes = [1.. ]

-- 「エラトステネスのふるい」というアルゴリズム
-- [2,3,4,5]のような並びであれば、まずは2を最初の素数として
-- 他の要素を2で割る、割り切れたら素数ではないので除外
-- これを繰り返す
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest
