clone :: Int -> [a] -> [a]
clone 0 _ = []
clone _ [] = []
clone n (x:xs) = replicate n x ++ clone n xs