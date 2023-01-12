-- non-tail recursion (bad)
binPow :: Int -> Int -> Int
binPow x n
    | n == 0    = 1
    | even n    = binPow (x ^ 2) (div n 2)
    | otherwise = binPow x (n - 1) * x

-- with tail recursion (good)
binPowHelper x n p
    | n == 0    = p
    | even n    = binPowHelper (x^2) (div n 2) p
    | otherwise = binPowHelper x (n - 1) (p * x)

binPowT x n = binPowHelper x n 1