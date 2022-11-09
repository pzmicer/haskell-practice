import Prelude hiding (Right, Left, Either)


data Pair a = Pair a a
data Labelled e a = Labelled e a
data OneOrTwo a = One a | Two a a
data Either e a = Left e | Right a
data MultiTree a = Leaf | Node a [MultiTree a]
data Stream a = Cons a (Stream a)


instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Labelled e) where
    fmap f (Labelled e a) = Labelled e (f a)

instance Functor OneOrTwo where
    fmap f (One a) = One (f a)
    fmap f (Two a b) = Two (f a) (f b)

-- ???
instance Functor (Either e) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

instance Functor MultiTree where
    fmap f Leaf = Leaf
    fmap f (Node a l) = Node (f a) (map (fmap f) l)
    
instance Functor Stream where
    fmap f (Cons a s) = Cons (f a) (fmap f s)
