import Data.List

main :: IO ()
main = print $ maximum products

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

products :: [Int]
products = nub [i * j | i <- [100..999], j <- [100..999], isPalindrome (show (i*j))]
