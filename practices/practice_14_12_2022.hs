import Distribution.Simple.Utils (xargs)


prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x:xs) = [] : map (x:) (prefixes xs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

shortestOddPrefix :: Integral a => [a] -> Maybe [a]
shortestOddPrefix xs = safeHead $ filter (odd . sum) $ prefixes xs

shortestOddPrefix2 :: Integral a => [a] -> Maybe [a]
shortestOddPrefix2 [] = Nothing
shortestOddPrefix2 (x:xs)
    | odd x = Just [x]
    | otherwise = fmap (x:) (shortestOddPrefix2 xs)

shortestOddPrefix3 :: Integral a => [a] -> Maybe [a]
shortestOddPrefix3 l = reverse <$> go l [] 0
    where
        go (x:xs) t s
            | odd s     = Just t
            | otherwise = go xs (x:t) (s+x)
        go [] t s
            | odd s     = Just t
            | otherwise = Nothing

