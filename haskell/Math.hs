module Math (fac, gcdLC, inverseModuloN, derivative, definiteIntegral, integral, permutationsN, permutations, collectionsN, collections, Group, Ring, Field, Vectorspace, Rn) where 

import           Data.Maybe
import           Sort


class Group m => Ring m where
  -- m <#> rempty = rempty <#> m = m  
  rempty :: m 
  --m1 <#> (m2 <+> m3) = (m1 <#> m2) <+> (m1 <#> m3)
  --(m1 <+> m2) <#> m3 = (m1 <#> m3) <+> (m2 <#> m3)
  (<#>)  :: m -> m -> m 

--gotta think up better names, lol.
class Ring m => Field m where 
  finverse :: m -> m

--Want to change sig to Field a => a -> m -> m. Compiler was complaining. Changed it to double to see if this works.
class Group m => Vectorspace m where 
  (#) :: Double -> m -> m 


instance Semigroup Int where 
  (<>) = (+)

instance Monoid Int where 
  mempty = 0

instance Group Int where 
  (<+>) = (<>)
  ginverse x = -x
  gempty = mempty 

instance Ring Int where 
  (<#>) = (*)
  rempty = 1 

instance Semigroup Double where 
  x <> y  = x + y

instance Monoid Double where 
  mempty = 0

instance Group Double where 
  (<+>) = (<>)
  ginverse x = -x
  gempty = mempty 

instance Ring Double where 
  (<#>) = (*)
  rempty = 1 

instance Field Double where 
  finverse x 
    | x == 0    = 0
    | otherwise = 1/x


newtype Rn = Rn [Double] 
--coordsRn :: Rn -> [Double]
--coordsRn (Rn xs) = xs 

instance Show Rn where 
  show (Rn xs) = "Rn " ++ show xs 

instance Semigroup Rn where 
  (Rn xs) <> (Rn ys)  = Rn (zipWith (+) xs ys)

instance Monoid Rn where 
  mempty = Rn (repeat 0)

instance Group Rn where 
  (Rn xs) <+> (Rn ys) 
    | length xs == length ys = Rn xs <> Rn ys
    | otherwise              = error "Vectors must have the same dimension."

  ginverse (Rn xs) = Rn (map negate xs)
  gempty = mempty 

instance Vectorspace Rn where 
  x # (Rn ys) = Rn [x*y | y <- ys]

--just so I can print fac n / fac k 
fac ::(Eq a, Fractional a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)


binaryCollections :: Int -> [[Int]]
binaryCollections n = collectionsN n [0,1]

(\\) :: (Eq a) => [a] -> [a] -> [a] 
(\\) xs [] = xs 
(\\) [] _  = [] 
(\\) xs (y:ys) = filter (/= y) xs \\ ys 

naiveSearch :: (Eq a) => [a] -> [a] -> Bool 
naiveSearch [] [] = True 
naiveSearch _  [] = False 
naiveSearch [] _  = True
naiveSearch (x:xs) (y:ys)
  | length xs > length ys  = False 
  | x == y                 = naiveSearch xs     ys
  | otherwise              = False 

foldr' :: ([a] -> [b] -> Bool) -> Bool -> [a] -> [b] -> Bool
foldr' _ empt _ [] = empt
foldr' op empt xs ys = op xs ys || foldr' op empt xs (tail ys)

sublist :: (Eq a) => [a] -> [a] -> Bool 
sublist = foldr' naiveSearch False


-- returns true if all elements in xs are in ys, irrespective of how they are placed in the list. 
inList :: (Eq a) => [a] -> [a] -> Bool 
inList [] ys = True
inList xs [] = False 
inList (x:xs) ys = x `elem` ys && inList xs ys 


pairConcat ::[(a,a)] -> [a] 
pairConcat (y:ys) 
  | null ys   = [fst y, snd y]
  | otherwise = fst y: pairConcat ys

filterDuplicates :: Ord a => [a] -> [a]
filterDuplicates xs = pairConcat . filter (uncurry (/=)) $ zip oxs (tail oxs)
  where oxs          = quicksort xs

_gcdTrail :: Int -> Int -> [[Int]] -> [[Int]]
_gcdTrail 0 _ res = res 
_gcdTrail _ 0 res = res 
_gcdTrail x y res 
  | x < y     = let q = y `quot` x  
                    r = y - q * x 
                 in _gcdTrail x r ([x,q,r]: res)
  | otherwise = let q = x `quot` y
                    r = x - q*y
                 in _gcdTrail y r ([y,q,r]:res)

--returns a list of remainder tuples from gcd algorithm
gcdTrail :: Int -> Int -> [[Int]]
gcdTrail x y 
  | x < y     = _gcdTrail y x [[y,0,x]] 
  | otherwise = _gcdTrail x y [[x,0,y]]

_backTrail :: [[Int]] -> [[Int]] 
_backTrail (x:xs) 
  | null xs   = xs
  | otherwise =
    [1, head $ head xs, -(x !! 1), head x] : _backTrail xs 

--r = u*x + v*y, then backTrail returns an array of arrays like [u,x,v,y], one entry for each residue.
backTrail :: Int -> Int -> [[Int]]
backTrail x y = _backTrail $ tail $ gcdTrail x y 


_gcdLC :: [[Int]] -> [Int]
_gcdLC [] = []
_gcdLC (x:xs) 
  | null xs   = x
  | otherwise =  let u1 = x  !! 0 
                     x1 = x  !! 1 
                     v1 = x  !! 2 
                     y1 = x  !! 3 
                     _x = head xs 
                     u2 = _x !! 0 
                     x2 = _x !! 1 
                     v2 = _x !! 2 
                     y2 = _x !! 3 
                  in 
                     if u2*x2 + v2*y2 == y1
                        then _gcdLC ([(v1*v2)+u1,  x1,      v1*u2, x2]: tail xs)
                        else _gcdLC ([u1*u2     ,  x2, (u1*v2)+v1, y1]: tail xs)

--Returns a list [u,x,v,y] such that g = gcd x y and u*x + v*y = g
gcdLC :: Int -> Int -> [Int]
gcdLC x y = _gcdLC $ backTrail x y

--Given a,n find k such that ak=n 
inverseModuloN :: Int -> Int -> Maybe Int  
inverseModuloN _ 0 = Nothing 
inverseModuloN 1 n = Just 1 
inverseModuloN a n 
  |gcd a n /= 1 = Nothing 
  | otherwise    = let d  = gcdLC a n 
  in if d !! 1 == n 
        then Just $ (d !! 2) `mod` n 
        else Just $ (d !! 0) `mod` n

derivativeApprox :: Float -> (Float -> Float) -> Float -> Float 
derivativeApprox h f x = (f (x + h) - f x) / h

derivative :: (Float -> Float) -> Float -> Float 
derivative = derivativeApprox 0.001

definiteIntegralApprox :: Float -> (Float -> Float) -> Float -> Float -> Float 
definiteIntegralApprox n f a b = dx * sum [f(a + (i-1) * dx) | i <- [1..n]]
  where dx = (b-a)/n

definiteIntegral :: (Float -> Float) -> Float -> Float -> Float 
definiteIntegral = definiteIntegralApprox 10000

integral :: (Float -> Float) -> Float -> Float 
integral f = definiteIntegral f 0

_permutationsN :: (Ord a , Eq a) => Int -> [a] -> [a] -> [[a]]
_permutationsN _ [] _  = [] 
_permutationsN 0 _ _   = [] 
_permutationsN _ _  [] = []
_permutationsN 1 xs _  = copies 1 xs 
_permutationsN k xs (y:ys) =  ((:) <$> [y] <*> _permutationsN (k-1) rest rest) ++ _permutationsN k xs ys 
  where rest = filter (/= y) xs 

permutationsN :: (Ord a , Eq a) => Int -> [a] -> [[a]]
permutationsN k xs = _permutationsN k xs xs 

permutations :: (Ord a, Eq a) => [a] -> [[a]]
permutations xs = permutationsN (length xs) xs
--

combinationsN :: (Ord a) => Int -> [a] -> [[a]]
combinationsN 0 xs    = [[]]
combinationsN _ []    = []
combinationsN 1 xs    = copies 1 xs 
combinationsN k (x:xs) = map (x :) (combinationsN (k - 1) xs) ++ combinationsN k xs 

--5. The numbers 1,2,..., are arranged randomly. Find the probability that the digits 
--   1,2...,k (k<n) appear as neighbors in that order.
--   Just filter list checking if a sublist exists. The sublist never changes. It's always [1,2,3,...,k]
sol5 :: Int -> Int -> Float 
sol5 k n = fnum / fdenom
  where prms  = permutations [1..n]
        num   = length $ filter (sublist [1..k]) prms 
        fnum  = fromIntegral num :: Float 
        denom = length prms 
        fdenom = fromIntegral denom :: Float 

--6. A pin table has seven holes through wihch a ball can drop. Five balls are played. Assuming that at each play a ball is equally likely to go down any one of the 
--   seven holes, find the probability that more than one ball goes down at least one of the holes. 
--   I think its 1 - nCk/n^k, lets see if we can write a program that explicitly computes lists of the possible outcomes 

copies :: (Eq a, Ord a) => Int -> [a] -> [[a]] 
copies k = map (replicate k)

collectionsN :: (Eq a, Ord a) => Int -> [a] -> [[a]]
collectionsN 0 _ = [] 
collectionsN 1 xs = copies 1 xs 
collectionsN n xs = concatMap (\x -> (:) <$> [x] <*> rest) xs  
  where rest = collectionsN (n-1) xs 

collections :: (Eq a, Ord a) => [a] -> [[a]]
collections xs = collectionsN (length xs) xs 

--eqList [a_1,...,a_n] == True if a_i == a_j for all i,j, else False.
eqList :: (Eq a) => [a] -> Bool 
eqList [] = True 
eqList [x] = True 
eqList (x:y:xs) = x == y && eqList (y:xs)

repetitions :: (Ord a, Eq a) => [a] -> Bool 
repetitions xs =  any (uncurry (==)) $ zip oxs (tail oxs)
  where oxs = quicksort xs 

sol6 :: Int -> Int -> Float 
sol6 k n = twoOrMoreF / allF
  where        
        cks        = collectionsN k [1..n]
        twoOrMore  = length $ filter repetitions cks
        all        = length cks
        twoOrMoreF = fromIntegral twoOrMore :: Float 
        allF       = fromIntegral all :: Float 

--Yeah, the answer is 1 - nCk/n^k, with k=5, n=7... one can explicitly check that with sol6 5 7.

-- 7. If 2n boys are divided into two equal subgroups find the probability that the two tallest boys will be (a) 
--    in different subgroups and (b) in the same subgroup.
sol7 :: Int -> (Float, Float)
sol7 n = (num1/den, num2/den)
  where xs = map (splitAt n) $ permutations [1..2*n]
        pred1 x = inList [1,2] x 
        pred2 (x,y) = not (pred1 x) && not (pred1 y)
        num1 = fromIntegral ((*2) . length . filter pred1 . map fst $ xs) :: Float 
        num2 = fromIntegral (length $ filter pred2 xs) :: Float 
        den = fromIntegral (length xs) :: Float 

--The combinatorics gives fst (sol7 n) = n-1 / (2n-1).
--                        snd (sol7 n) = 1 - (fst (sol7 n)) = n/(2n-1)
--which matches sol7's output.
--filter (\(x,y) -> (not (inList [1,2] x)) && (not (inList [1,2] y))) . map (splitAt 2) . permutations $ [1..4]

-- 8. In a movie theater that can accomodate n + k people, n people are seated. What is the probability that r <= n given seats are occupied. 
sol8_1 :: Int -> Int -> Int -> Float 
sol8_1 r n k = num/den
  where nums   = [1..n+k]
        cxs    = combinationsN n nums 
        guess  = filter (sublist [1..r]) cxs 
        num    = fromIntegral (length guess) :: Float 
        den    = fromIntegral (length cxs)   :: Float 

seats :: Int -> Int -> [([Int], [Int])]
seats n k = map (\x -> (x, nums \\ x)) $ combinationsN n nums
  where nums = [1..n+k]

--Turns out that this can be represented by nCr/(n+k)Cr, which prompted another way to do this
sol8_2 :: Int -> Int -> Int -> Float 
sol8_2 r n k = num/den
  where cxs = permutationsN r [1..n] 
        pxs = permutationsN r [1..n+k]
        num = fromIntegral (length cxs) :: Float 
        den = fromIntegral (length pxs) :: Float 

--9. Waiting in line for a Saturday morning movie show are 2n children. Tickets are priced at a quarter each. Find the probability that nobody
--   will have to wait for change if, before a ticket is sold to the first customer, the cashier has 2k (k < n) quarters. Assume that it is 
--   equally likely that each ticket is paid for with a quarter or a half-dollar coin. 

data Money = Quarter | Halfdollar
  deriving (Show, Eq)

type Cashier = [[Money]]

makeCashier :: Int -> Int -> Cashier 
makeCashier n k = [replicate n Quarter, replicate k Halfdollar]

--need to generate all possible binary numbers up to n...
possibleTransactions :: Int -> [[Money]]
possibleTransactions n = map (map (\x -> if x == 0 then Quarter else Halfdollar)) $ binaryCollections n

--returns state of cashier. if the transaction fails, returns Nothing.
transaction :: Cashier -> Money -> Maybe Cashier 
transaction [[],_] Halfdollar   = Nothing 
transaction [qs, hs] Quarter    = Just [Quarter:qs, hs]
transaction [qs,hs] Halfdollar  = Just [drop 1 qs, Halfdollar : hs]

--Takes a list of money, and executes all of the transactions. If one fails, returns nothing. 
transactions :: Cashier -> [Money] -> Maybe Cashier 
transactions c []      = Just c 
transactions c (x:xs)  = case transaction c x of 
                           Nothing       -> Nothing 
                           Just c'       -> transactions c' xs

outcomes :: Int -> Int -> [Maybe Cashier]
outcomes n k =  map (transactions cash) trans
  where trans = possibleTransactions  n
        cash  = makeCashier k 0 
sol9 n k = num/denom
  where xs    = outcomes (2*n) (2*k)
        num   = fromIntegral $ length (filter isJust xs) :: Float 
        denom = fromIntegral $ length xs :: Float 

--10. Each box of a certain brand of breakfast cereal contains a small charm, with k distinct charms forming a set. Assuming that the chance of drawing any particular charm is equal to that of drawing any other charm, show that the probability of finding at least one complete set of charms in a random purchase of N >= boxes equals
--1 - |k|((k-1)/k)^N + |k|((k-2)/k)^N - |k|((k-3)/k)^N  + ... + (-1)^(k-1)|k  |(1/k)^N
--    |1|              |2|              |3|                               |k-1|
--Hint: Use (1.3.6)

allCharms :: Int -> Int -> [[Int]]
allCharms n k = collectionsN k [1..n]

setCharms :: Int -> Int -> [[Int]]
setCharms n k = filter (inList [1..n]) (allCharms n k)

sol10 n k = num/denom 
  where xs    = setCharms n k 
        ys    = allCharms n k 
        num   = fromIntegral $ length xs :: Float 
        denom = fromIntegral $ length ys :: Float 

class Monoid m => Group m where
    gempty   :: m 
    (<+>)    :: m -> m -> m 
    ginverse :: m -> m
    

newtype (Transpose a) = Transpose
  { runTranspose :: a -> a
  } 

newtype Permutation a = Permutation [Transpose a]

instance Semigroup (Transpose a) where 
  (Transpose s1) <> (Transpose s2)   = Transpose (s1 . s2)

instance Monoid (Transpose a) where 
  mempty = Transpose id

instance Group (Transpose a) where 
  gempty = mempty 
  (<+>) = (<>)
  --a transposition is its own inverse
  ginverse (Transpose f) = Transpose f

transposeIJ :: (Eq a) => a -> a -> Transpose a 
transposeIJ i j = Transpose _transposeIJ
  where 
    _transposeIJ x
      | x==i      = j 
      | x==j      = i 
      | otherwise = x 

instance Semigroup (Permutation a) where 
  (Permutation p1) <> (Permutation p2) = Permutation (p1++p2)

instance Monoid (Permutation a) where 
  mempty = Permutation [] 

instance Group (Permutation a) where 
  gempty = mempty 
  (<+>) = (<>)
  ginverse (Permutation xs) = Permutation (reverse xs)

runPermutation :: Permutation a -> a -> a 
runPermutation (Permutation []) = id 
runPermutation (Permutation (t:ts)) = runTranspose t . runPermutation (Permutation ts) 

toTranspose :: (Eq a) => [a] -> [Transpose a]
toTranspose [] = [] 
toTranspose [x] = [gempty]
toTranspose (x:y:ys) = transposeIJ x y : toTranspose (y:ys)

permutation :: (Eq a) => [a] -> Permutation a 
permutation xs = Permutation (toTranspose xs)


