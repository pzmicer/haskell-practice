data Tree a 
    = Empty
    | Node a (Tree a) (Tree a)
    deriving(Show)

count :: Tree a
count = undefined


-- BFS
--                 f                Acc  Queue       Result
traverseWHelper :: (a -> b -> b) -> b -> [Tree a] -> b
traverseWHelper _ b [] = b
traverseWHelper f b (Empty:ts) = traverseWHelper f b ts
traverseWHelper f b ((Node a l r):ts) = traverseWHelper f (f a b) (ts ++ [l, r])

treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseW f b t = traverseWHelper f b [t]
    

-- DFS
--               f                Acc   Tree     Result
treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node v l r) = treeTraverseD f (f v (treeTraverseD f acc l)) r