import Math

main :: IO ()
main = print $ sum $ takeWhile (< 2000000) primes
