main :: IO ()
main = print $ answer 600851475143

factors :: Int -> [Int]
factors n = low ++ high
    where
        s = (floor . sqrt . fromIntegral) n
        low = [i | i <- [2..s], n `mod` i == 0]
        high = if low == [head low]
            then []
            else [n `div` i | i <- low]

isPrime 1 = False
isPrime n = factors n == []

answer :: Int -> Int
answer n = maximum $ filter isPrime (factors n)
