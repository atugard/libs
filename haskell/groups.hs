group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) = a:(group b) 
  where (a,b) = span (==x x:xs)
