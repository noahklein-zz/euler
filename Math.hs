module Math (
    digits,
    fact
    ) where

import Data.List

digits :: Integer -> [Integer]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
