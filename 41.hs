import Data.List
import Data.Numbers.Primes (isPrime)

main :: IO ()
main = print $ answer pandigitals

pandigitals = concatMap (nDigitPandigital) [9, 8..1]

primePandigitals = filter isPrime pandigitals

nDigitPandigital :: Int -> [Int]
nDigitPandigital n = map listToNum (permutations [1..n])

listToNum :: [Int] -> Int
listToNum = sum . zipWith (\dig n -> n * 10 ^ dig) [0..]

answer = head . reverse . sort