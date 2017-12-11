main :: IO ()
main = print $ answer 1000

answer :: Int -> Int
answer n = (`mod` 10^10) $ sum $ map selfPower [1..n]

selfPower :: Int -> Int
selfPower n = n ^ n
