module Math (
    primes,
    digits,
    fact
    ) where

import Data.List

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]

sieve :: [Int] -> [Int] -> [Int]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `mod` p /= 0]
  where (h, ~(_:t)) = span (< p * p) xs

digits :: Integer -> [Integer]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
