import Data.List
import Data.Char
import Data.Numbers.Primes

answer :: Int
answer = 2 + (length $ filter isCircularPrime (primesToCheck 1000000))

isCircularPrime :: Int -> Bool
isCircularPrime = all isPrime . rotations

rotations :: Int -> [Int]
rotations = map digitsToInt . _rotations . digits
    where _rotations xs = init $ zipWith (++) (tails xs) (inits xs)

primesToCheck :: Int -> [Int]
primesToCheck n  = takeWhile (< n) (filter badNum primes)

badNum :: Int -> Bool
badNum = all digitCheck . digits
    where digitCheck d = odd d && d /= 5

digits :: Int -> [Int]
digits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc n -> acc * 10 + n) 0
