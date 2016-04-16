main :: IO ()
main = print $ answer 100

sumOfSquares n = sum $ map (\x -> x**2) [1..n]

squareOfSum n = (sum [1..n])**2

answer n = (squareOfSum n) - sumOfSquares n
