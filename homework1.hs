factorial'guard :: Integer -> Integer 
factorial'guard n 
    | n==0      = 1
    | otherwise = n * factorial'guard (n-1)

factorial'if :: Integer -> Integer 
factorial'if n = if n == 0
                   then 1
                   else n * factorial'if (n-1)

factorial'case :: Integer -> Integer 
factorial'case n = case n of 0 -> 1
                             othewise -> n * factorial'case (n-1)

leapyear :: Int -> String
leapyear y 
    | y `mod` 400 == 0 = "Leap year"
    | y `mod` 100 == 0 ="Not leap year"
    | y `mod` 4   == 0 = "Leap year"
    | otherwise        ="Not leap year"
 
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

leapyear' :: Int -> Bool
leapyear' y = (y `mod` 4 ==0) && ( not (y `mod` 100 ==0)) || (y `mod` 400==0) 

{- homework 1.3 proof
(a) fst.pair (f,g) x
    = fst ( pair(f,g) x )
    = fst ( f x ,g x)
    = f x

(b) snd.pair (f,g) x
    = snd ( pair(f,g) x )
    = snd ( f x ,g x)
    = g x

(c) pair (f,g) .h x
    = pair (f,g) (h x)
    = ( f(h x) ,g(h x) )
    = ( f.h x ,g.h x) 
    = pair (f.h ,g.h)

(e) cross (f,g) .cross(h,k) (x,y)
    = cross (f,g) .cross (h,k) (x,y))
    = cross (f,g) ( pair (h.fst ,k.snd) (x,y))
    = cross (f,g) ( ( h(fst (x,y) ), k( snd(x,y))))
    = cross (f,g) ( (h x ,k y))
    = pair (f.fst ,g.snd) ( h x ,k y)
    = ( f( fst(h x ,k y)) , g( snd(h x ,k y)))
    = ( f( h x) , g( k y))
    = ( f.h x ,h.k y)
    = ( f.h (fst(x,y)) ,g.k (snd(x,y)))
    = pair ( (f.h).fst ,(g.k).snd) (x,y)
    = cross (f.h ,g.k ) (x,y)
-}
