collatz 1 = 0
collatz x
    | even x    = (+1) $ collatz $ div x 2
    | otherwise = (+1) (3 * x + 1)

--

collatzHelper x n
    | x == 1    = n
    | even x    = collatzHelper (div x 2) (n + 1)
    | otherwise = collatzHelper (3*x + 1) (n + 1)

collatzT x = collatzHelper x 0

--

syracuse x
    | x == 1    = []
    | even x    = x : syracuse (div x 2)
    | otherwise = x : syracuse (3*x + 1)