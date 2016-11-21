import Data.List

main :: IO ()
main = print $ firstTriangleDivisibleBy 500

triangles :: [Int]
triangles = scanl (+) 1 [2..]

numDivisors :: Int -> Int
numDivisors = (*2) . length . divisors
  where
    divisors n = [x | x <- [1..s], n `mod` x == 0]
        where
            s = (floor . sqrt . fromIntegral) n


firstTriangleDivisibleBy :: Int -> Maybe Int
firstTriangleDivisibleBy n = find ((> n) . numDivisors) triangles
