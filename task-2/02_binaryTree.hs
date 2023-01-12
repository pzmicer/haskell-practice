data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving(Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
    | x <= v    = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

buildHelper :: Ord a => [a] -> Tree a -> Tree a
buildHelper xs t = foldl (flip insert) t xs

build :: Ord a => [a] -> Tree a
build l = buildHelper l Empty

--                        f                Acc  Queue       Result
traversalBreadthHelper :: (a -> b -> b) -> b -> [Tree a] -> b
traversalBreadthHelper _ b [] = b
traversalBreadthHelper f b (Empty:ts) = traversalBreadthHelper f b ts
traversalBreadthHelper f b ((Node a l r):ts) = traversalBreadthHelper f (f a b) (ts ++ [l, r])

traversalBreadth :: (a -> b -> b) -> b -> Tree a -> b
traversalBreadth f b t = traversalBreadthHelper f b [t]

--                f                Acc  Tree      Result
traversalDepth :: (a -> b -> b) -> b -> Tree a -> b
traversalDepth _ acc Empty = acc
traversalDepth f acc (Node v l r) = traversalDepth f (f v (traversalDepth f acc l)) r

size :: Tree a -> Int
size = traversalDepth (\ a b -> b+1) 0

search :: Eq a => a -> Tree a -> Bool
search s Empty = False
search s (Node v l r)
    | s == v = True
    | search s l = True
    | search s r = True
    | otherwise = False