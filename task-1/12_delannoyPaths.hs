delannoyStep :: [[Int]] -> [[Int]] -> [[Int]]
delannoyStep cs ds = ([(0 :), (2 :)] <*> ds) ++ ([(1 :)] <*> cs)

delannoyHelper :: [[Int]] -> [[Int]] -> [[[Int]]]
delannoyHelper cs ds = cs : delannoyHelper ds (delannoyStep cs ds)

delannoyLayers :: [[[Int]]]
delannoyLayers = delannoyHelper [[]] [[0], [2]]

-- A x B grid, where a - vertical length, b - horizontal length
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = filter (\l -> pathTo l == (a, b)) (delannoyLayers !! (a + b))

pathTo :: [Int] -> (Int, Int)
pathTo = foldl f (0, 0)
  where
    f (a, b) x
      | x == 0 = (a, b + 1)
      | x == 1 = (a + 1, b + 1)
      | otherwise = (a + 1, b)
