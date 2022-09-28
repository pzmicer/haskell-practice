getDelims :: Int -> [Int]
getDelims n = [ d | d <- [1..n-1], mod n d == 0]

isPerfect :: Int -> Bool
isPerfect n = sum (getDelims n) == n
