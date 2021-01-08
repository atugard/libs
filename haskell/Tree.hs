module Tree (Tree) where 

flatten :: Tree a -> [a]
flatten Leaf = [] 
flatten (Node x []) = [x]
flatten (Node x xs) = x : concatMap flatten xs

data Tree a = Leaf | Node a [Tree a]
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Leaf     = Leaf
  fmap f (Node x [])  = Node (f x) []
  fmap f (Node x xs)  = Node (f x) (map (fmap f) xs)

instance Applicative Tree where 
  pure x                      = Node x [] 
  Leaf    <*> _           = Leaf 
  _           <*> Leaf    = Leaf 
  (Node f []) <*> (Node y _)  = Node (f y) []
  (Node f _)  <*> (Node y []) = Node (f y) []
  (Node f xs) <*> (Node y ys) = Node (f y) (zipWith (<*>) xs ys)

instance Monad Tree where 
  --(>>=) :: m a -> (a -> m b) -> m b 
  Leaf  >>= _ = Leaf
  Node x [] >>= f = f x 
  Node x xs >>= f = 
    case f x of 
      Leaf  -> Leaf
      Node y ys -> Node y $ map (>>= f) xs ++ ys

instance Foldable Tree where 
  foldr f null t = foldr f null (flatten t)

instance Traversable Tree where 
  traverse _ Leaf = pure Leaf
  traverse f (Node x xs) = Node <$> f x <*> traverse (traverse f) xs  

