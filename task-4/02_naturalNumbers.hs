import Data.Set as Set


class NaturalSet f where
    isMember :: (Eq a) => a -> f a -> Bool
    toList :: f a -> [a]
    fromList :: [a] -> f a
    union :: (Eq a) => f a -> f a -> f a
    intersection :: (Eq a) => f a -> f a -> f a
    difference :: (Eq a) => f a -> f a -> f a


instance NaturalSet [] where
  isMember = elem
  
  toList = id
  
  fromList = id
  
  union l1 l2 = go l1 l2
    where
        go l1 [] = l1
        go l1 (x:xs)
            | x `elem` l1 = go l1 xs
            | otherwise = go (x:l1) xs
  
  intersection [] _ = []
  intersection _ [] = []
  intersection l1 l2 = go l1 l2 []
    where
        go [] l2 r = r
        go (x:xs) l2 r
            | x `elem` l2 = go xs l2 (x:r)
            | otherwise = go xs l2 r
  
  difference = undefined


