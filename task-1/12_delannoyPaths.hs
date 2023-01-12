delannoyStep :: [[Int]] -> [[Int]] ->[[Int]]
delannoyStep cs ds = ([(0:), (2:)] <*> ds) ++ ([(1:)] <*> cs)

delannoyHelper :: [[Int]] -> [[Int]] ->[[[Int]]]
delannoyHelper cs ds = cs : delannoyHelper ds (delannoyStep cs ds)

delannoyLayers :: [[[Int]]]
delannoyLayers = delannoyHelper [[]] [[0], [2]]

-- It is not working yet
-- a x b grid, where a  vertical length, b - horizontal length
delannoyPaths a b = filter (\l -> count 0 l <= b && count 2 l <= a) (delannoyLayers !! (a + b))

count :: Int -> [Int] -> Int
count x = length . filter (x==)