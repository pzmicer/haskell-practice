xZipWith :: (a->b->c) -> [a] -> [b] -> [c]
xZipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = f x y : go xs ys