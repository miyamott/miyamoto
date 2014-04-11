{- A function that to make Fibonacci series due to give a numerical range to argument -}
fibonacci :: [ Int ] -> [ Integer ]
fibonacci n = [ f m | m <- n ] 
    where f 0 = 1
          f 1 = 1
          f m = f (m-1) + f(m-2) 
{- A function that to make a infinite list of Fibonacci series -}
fibonacci' :: [ Integer ]
fibonacci' = [ f m | m <- [0..] ] 
    where f 0 = 1
          f 1 = 1
          f m = f(m-1) + f(m-2) 

{- Euclid algorithm -}
euclidean :: Integral a => a -> a -> a
euclidean a 0 = a
euclidean a b = euclidean b (a `mod` b) 

{- flatten a given list of list into list ver.1 -}
flat :: [[a]] -> [a]
flat (x:[]) = x
flat (x:xs) = x ++ flat xs

{- flatten a given list of list into list ver.2 -}
flat' :: [[a]] -> [a]
flat' [[]] = []
flat' (x:xs) = x ++ flat xs

