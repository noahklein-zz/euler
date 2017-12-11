import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.Set as Set


main :: IO ()
main = do
    let ans = sum $ filter (not . checkNum) [1..28123]
    print ans
    return ()


divisors ::Int -> [Int]
divisors n = 1:divs
    where
        possibles = [2..floor . sqrt $ (fromIntegral n :: Float)]
        divs = possibles >>= (\x ->
            if n `mod` x == 0
                then
                    (if x == n `div` x then [x] else [x, n `div` x])
                else []
            )

isAbundant :: Int -> Bool
isAbundant n = n < (sum $ divisors n)

abundants :: Set.Set Int
abundants = Set.fromList $ filter isAbundant [1..28123]

checkNum :: Int -> Bool
checkNum n = isJust $ L.find f abundants
    where f x = Set.member (n - x) abundants
