import Math

main :: IO ()
main = print $ answer 10001

answer :: Int -> Int
answer n = last . (take n) $ primes
