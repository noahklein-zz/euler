main :: IO ()
main = print $ answer 4000000

fibs :: [Int]
fibs = 1:2:(zipWith (+) fibs (tail fibs))

answer :: Int -> Int
answer upper = compute fibs
    where
        compute = sum . (filter even) . (takeWhile (<= upper))
