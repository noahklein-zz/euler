multiples :: Int -> [Int] 
multiples upper =
    [i | i <- [1..upper], i `mod` 5 == 0 || i `mod` 3 ==0]

answer :: Int -> Int
answer n = sum $ multiples (n-1) 
