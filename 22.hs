import Data.List.Split
import Data.List

main :: IO ()
main = do
    fileContents <- readFile "files/p022_names.txt"
    let names = fileToNames fileContents
    print $ fileScore $ sort names

fileToNames :: String -> [String]
fileToNames = sort . splitOn "," . filter (/= '\"')

fileScore :: [String] -> Int
fileScore = sum . zipWith (*) [1..] . map nameScore

nameScore :: String -> Int
nameScore = sum . map letterScore

letterScore :: Char -> Int
letterScore l = case elemIndex l ['A'..'Z'] of
    Just n -> n + 1
    _      -> 0
