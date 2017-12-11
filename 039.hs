import Data.List

main :: IO ()
main = print answer

answer :: Int
answer = a + b + c
    where (a, b, c) = (head . mostSolutions) allSolutions

solutions :: Int -> [(Int, Int, Int)]
solutions p = [(a, b, c) |
    a <- [1..p `div` 3],
    let b = (p^2 - 2 * p * a) `div` (2 * p - 2 * a),
    let c = p - (a + b),
    p `mod` 3 == 0,
    (p^2 - 2 * p * a) `mod` (2 * p - 2 * a) == 0,
    a^2 + b^2 == c^2]

allSolutions :: [[(Int, Int, Int)]]
allSolutions = map solutions [2, 4..1000]

mostSolutions :: [[(Int, Int, Int)]] -> [(Int, Int, Int)]
mostSolutions = maximumBy (\a b -> compare (length a) (length b))
