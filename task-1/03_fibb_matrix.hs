import Data.List (transpose)

-- Matrix multiplication function
mult :: [[Int]] -> [[Int]] -> [[Int]]
mult m1 m2 = [[ sum $ zipWith (*) m1r m2c | m2c <- transpose m2] | m1r <- m1]

-- Bad bacause of non-tail recursion
powerMatrix :: [[Int]] -> Int -> [[Int]]
power [] n = []
powerMatrix m n
    -- | n == 0 = getIdentityMatrix $ length m
    | n == 1 = m
    | even n = powerMatrix m_x_m (div n 2)
    | odd n  = mult (powerMatrix m (n - 1)) m
    where
        m_x_m = mult m m

getIdentityMatrix n = [[if i == j then 1 else 0 | i <- [0..n]] | j <- [0..n]]