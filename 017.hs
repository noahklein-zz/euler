import qualified Data.Map.Strict as Map

main = do
    print $ 11 + evalNums [1..999]
    return ()

explodeDigits :: Int -> (Int, Int, Int)
explodeDigits n = (h, t, u)
    where
        (n', u)  = n `divMod` 10
        (n'', t) = n' `divMod` 10
        (_, h)   = n'' `divMod` 10

evalNum = length . numName . explodeDigits

evalNums = sum . map evalNum

numName :: (Int, Int, Int) -> String
numName (0, 0, u) = lookupName u
numName (0, t, 0) = lookupName (t * 10)
numName (0, t, u) = if t == 1
    then lookupName (t * 10 + u)
    else lookupName (t * 10) ++ lookupName u
numName (h, t, u) = lookupName h ++ "hundred" ++ and ++ numName (0, t, u)
    where
        and = if t > 0 || u > 0
            then "and"
            else ""


lookupName :: Int -> String
lookupName n = Map.findWithDefault "" n names

names :: Map.Map Int String
names = Map.fromList [
        (1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine"),
        (10, "ten"),
        (11, "eleven"),
        (12, "twelve"),
        (13, "thirteen"),
        (14, "fourteen"),
        (15, "fifteen"),
        (16, "sixteen"),
        (17, "seventeen"),
        (18, "eighteen"),
        (19, "nineteen"),
        (20, "twenty"),
        (30, "thirty"),
        (40, "forty"),
        (50, "fifty"),
        (60, "sixty"),
        (70, "seventy"),
        (80, "eighty"),
        (90, "ninety")
    ]
