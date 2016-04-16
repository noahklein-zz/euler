main :: IO ()
main = print $ answer 10001

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p:(sieve [i | i <- xs, i `mod` p /= 0])

answer :: Int -> Int
answer n = last . (take n) $ sieve (2:[3,5..])
