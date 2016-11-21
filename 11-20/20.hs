import Math (digits, fact)

answer :: Integer -> Integer
answer  = sum . digits . fact

main :: IO ()
main = print $ answer 100