-- Classes
data Example = Empty | Labelled Int String

instance Eq Example where
  Empty == Empty = True
  (Labelled x _) == (Labelled y _) = True
  _ == _ = False

-- Functors
l1 = [[1, 2, 3], [10, 20], [100], []]
l2 = fmap (fmap (+1)) l1