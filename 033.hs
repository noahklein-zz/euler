import Data.Ratio

main :: IO ()
main = do
  let answers = map (uncurry (%)) check
  print $ zip answers check
  print $ product $ map (uncurry (%)) check

digits :: Integral a => a -> (a, a)
digits = (`divMod` 10)

cancelDigits :: (Integer, Integer) -> [Rational]
cancelDigits (a, b) = map (uncurry (%)) $ dedupe $ filter (\(a, b) -> b /= 0 && a /= b) $ clear (digits a) (digits b)
  where
    clear (a1, a2) (b1, b2) = [
        f (a2 == b2) (a1, b1),
        f (a2 == b1) (a1, b2),
        f (a1 == b2) (a2, b1),
        f (a1 == b1) (a2, b2)
      ]
    f bool val = if bool then val else (0, 0)

options :: [(Integer, Integer)]
options = [(a, b) | a <- [10..99], b <- [a..99], ((> 0) . length . cancelDigits) (a, b), a `mod` 10 /= 0 || b `mod` 10 /= 0]

check ::[(Integer, Integer)]
check = filter f options
    where
      f d@(a, b) = ((> 0) .length) $ filter (== (a % b)) (cancelDigits d)

dedupe :: Eq a => [a] -> [a]
dedupe = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
