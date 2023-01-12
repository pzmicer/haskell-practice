xLookup :: Eq a => a -> [(a, b)] -> Maybe b
xLookup _ [] = Nothing
xLookup x (p:ps) = if k == x then Just v else xLookup x ps
    where (k, v) = p

data XList a
    = XNil  
    | XCons a (XList a)
    deriving (Show)

xFilter :: (a -> Bool) -> XList a -> XList a
xFilter _ XNil = XNil
xFilter p (XCons x xs)
    | p x = XCons x (xFilter p xs)
    | otherwise = xFilter p xs

xTake :: Int -> XList a -> XList a
xTake _ XNil = XNil
xTake n (XCons x xs)
    | n == 0    = XNil
    | otherwise = XCons x (xTake (n-1) xs)

toXList :: [a] -> XList a
toXList = foldr XCons XNil

toList :: XList a -> [a]
toList XNil = []
toList (XCons x xs) = x : toList xs
