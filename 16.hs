digits :: Integer -> [Integer]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

answer :: Integer -> Integer
answer = sum . digits . (2^)

main :: IO ()
main = print $ answer 1000
