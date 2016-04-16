divisibleByAll :: Int -> Bool
divisibleByAll n = all divisibleBy [11..19]
    where divisibleBy = (== 0) . (mod n)

findSmallestDivisibleByAll = head (filter divisibleByAll [20,40..])
