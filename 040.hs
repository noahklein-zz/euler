import Math (digits)

main = print answer

num = [1..] >>= (reverse . digits)

is = map (\p -> 10^p) [0..6]

answer = product $ map (\i -> num !! (i - 1)) is
