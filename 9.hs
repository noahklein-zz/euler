main :: IO ()
main = print $ product' (head triplets)

triplets :: [(Int, Int, Int)]
triplets = [(a,b,c) | a <- [1..1000], b <- [a..1000], isPerfectSquare (a*a + b*b), let c = sq (a*a+b*b), a+b+c == 1000]

isPerfectSquare :: Int -> Bool
isPerfectSquare n = (sq n) * (sq n) == n

sq = floor . sqrt . fromIntegral

product' :: (Int,Int,Int) -> Int
product' (a,b,c) = a*b*c
