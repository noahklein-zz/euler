module Math (
    primes
    ) where

import Data.List


primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]

sieve :: [Int] -> [Int] -> [Int]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `mod` p /= 0]  
  where (h, ~(_:t)) = span (< p * p) xs