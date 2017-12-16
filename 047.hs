import Data.Numbers.Primes (primeFactors)
import Data.List (find, group)

main :: IO ()
main = print $ answer 4

numPrimeFactors :: Int -> Int
numPrimeFactors = length . group . primeFactors

answer :: Int -> Int
answer l = head [n | n <- [2..], check [n..n+l-1]]
  where
    check xs = and [numPrimeFactors x == l | x <- xs]
