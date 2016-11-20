main :: IO ()
main = print $ change coins 200

coins = [1, 2, 5, 10, 20, 50, 100, 200]

change :: [Int] -> Int -> Int
change [] _ = 0
change _  0 = 1
change xxs@(x:xs) n
    | n >= x    = change xxs (n - x) + change xs n
    | otherwise = change xs n