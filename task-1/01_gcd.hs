myGcd :: Int -> Int -> Int
myGcd a b
    | a > b = myGcd (a - b) b 
    | a < b = myGcd a (b - a)
    | otherwise = a
