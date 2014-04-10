{- 引数に数のレンジを与えてFibonacci数列を作る関数-}
fibonacci :: [ Int ] -> [ Integer ]
fibonacci n = [ f m | m <- n ] 
    where f 0 = 1
          f 1 = 1
          f m = f (m-1) + f(m-2) 
{- Fibonacci数列の無限リストを作る関数-}
fibonacci' :: [ Integer ]
fibonacci' = [ f m | m <- [0..] ] 
    where f 0 = 1
          f 1 = 1
          f m = f(m-1) + f(m-2) 

{- Euclid の互除法 -}
euclidean :: Integral a => a -> a -> a
euclidean a 0 = a
euclidean a b = euclidean b (a `mod` b) 
