import Data.Numbers.Primes (isPrime)


main :: IO ()
main = print $ maxBy snd candidatesWithAnswers


quad :: Int -> Int -> [Int]
quad a b = [n^2 + a*n + b | n <- [0..]]

numConsecutivePrimes :: [Int] -> Int
numConsecutivePrimes = length . takeWhile isPrime

candidates :: [(Int, Int)]
candidates = [(a, b) | a <- [-1000..1000], b <- [-1000..1000]]

answers :: [Int]
answers = (map (numConsecutivePrimes . (uncurry quad)) candidates)

candidatesWithAnswers :: [((Int, Int), Int)]
candidatesWithAnswers = zip candidates answers

maxBy :: (Ord b) => (a -> b) -> [a] -> a
maxBy _ [] = error "Empty list not accepted"
maxBy _ [a] = a
maxBy f (a:b:xs) = maxBy f $ (if f a > f b then a else b):xs
