module Tree (Tree) where 

flatten :: Tree a -> [a]
flatten Nulltree = [] 
flatten (Node x []) = [x]
flatten (Node x xs) = x : concatMap flatten xs

data Tree a = Nulltree | Node a [Tree a]
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Nulltree     = Nulltree
  fmap f (Node x [])  = Node (f x) []
  fmap f (Node x xs)  = Node (f x) (map (fmap f) xs)

instance Applicative Tree where 
  pure x                      = Node x [] 
  Nulltree    <*> _           = Nulltree 
  _           <*> Nulltree    = Nulltree 
  (Node f []) <*> (Node y _)  = Node (f y) []
  (Node f _)  <*> (Node y []) = Node (f y) []
  (Node f xs) <*> (Node y ys) = Node (f y) (zipWith (<*>) xs ys)

instance Monad Tree where 
  --(>>=) :: m a -> (a -> m b) -> m b 
  Nulltree  >>= _ = Nulltree
  Node x [] >>= f = f x 
  Node x xs >>= f = 
    case f x of 
      Nulltree  -> Nulltree
      Node y ys -> Node y $ map (>>= f) xs ++ ys

instance Foldable Tree where 
  foldr f null t = foldr f null (flatten t)

instance Traversable Tree where 
  traverse _ Nulltree = pure Nulltree
  traverse f (Node x xs) = Node <$> f x <*> traverse (traverse f) xs  

