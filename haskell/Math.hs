import Data.Maybe 

import Sort 

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

derivative_approx :: Float -> (Float -> Float) -> Float -> Float 
derivative_approx h f x = (f (x + h) - f x) / h

derivative :: (Float -> Float) -> Float -> Float 
derivative = derivative_approx 0.001

definite_integral_approx :: Float -> (Float -> Float) -> Float -> Float -> Float 
definite_integral_approx n f a b = dx * sum [f(a + (i-1) * dx) | i <- [1..n]]
  where dx = (b-a)/n

definite_integral :: (Float -> Float) -> Float -> Float -> Float 
definite_integral = definite_integral_approx 10000

integral :: (Float -> Float) -> Float -> Float 
integral f x = definite_integral f 0 x

_permutations :: Eq a => [a] -> [a] -> [[a]]
_permutations [] _  = [] 
_permutations [x] _ = [[x]]
_permutations _  [] = []
_permutations xs (y:ys) =  ((:) <$> [y] <*> _permutations rest rest) ++ _permutations xs ys 
  where rest = filter (/= y) xs 

permutations :: Eq a => [a] -> [[a]]
permutations xs = _permutations xs xs 
