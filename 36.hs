import Math (digits)

main :: IO ()
main = do
    print $ sum [digitsToNum x | x <- binPalindromes]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- decPalindromes :: [[Int]]
decPalindromes = filter isPalindrome (map digits [1..1000000])

binPalindromes = filter (isPalindrome . toBin . digitsToNum) decPalindromes

toBin 0 = [0]
toBin n = reverse (helper n)
    where
        helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q

digitsToNum ds = helper ds 1
        where
            helper [] _     = 0
            helper (d:ds) n = (n * d) + helper ds (n * 10)
