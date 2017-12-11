import Data.Numbers.Primes

main :: IO ()
main = print $ answer 10001

answer :: Int -> Int
answer n = last . (take n) $ primes
