lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

lucky' :: Int -> String
lucky' x = if x == 7
            then "lucky"
            else "out of luck"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

firstLetter :: String -> String
firstLetter "" = " Empty"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ "is "++[x]

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underwight"
  | bmi <= 25.0 = "You're normal"
  | bmi <= 30.0 = "You're fat"
  | otherwise   = "whale"
 
bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= 18.5 = "You're underwight"
  | bmi <= 25.0 = "You're normal"
  | bmi <= 30.0 = "You're fat"
  | otherwise   = "whale"
  where bmi = weight / height^2

cylinder :: Double -> Double -> Double
cylinder r h = 
    let sideArea = 2 *pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [ Double]
calcBmis xs = [bmi | (w,h) <- xs , let bmi = w/ h^2]

head' :: [a] -> a
head' xs =case xs of [] -> error "No"
                     (x:_) -> x

add2vector :: (Double,Double) -> (Double,Double) -> (Double,Double)
add2vector a b = ( fst a + fst b, snd a + snd b)

add2vector' :: (Double,Double) -> (Double,Double) -> (Double,Double)
add2vector' (x1,y1) (x2,y2)  = ( x1+x2,y1+y2)

first :: (a,b,c) -> a
first (x,_,_) =x
second :: (a,b,c) -> b
second (_,y,_) =y
third ::(a,b,c) -> c
third (_,_,z) =z

add3vector :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
add3vector a b = ( first a + first b , second a + second b,third a+third b)

add3vector' :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
add3vector' (x1,y1,z1) (x2,y2,z2)  = ( x1+x2,y1+y2,z1+z2)

physicist = ["Einstein","Hawking","Friedman","Faynman","F",""]

head'' :: [a] -> a
head'' [] = error "Can't call head on an empty list ,dummy!"
head'' (x:_) =x

tell :: (Show a) => [a] -> String
tell [] ="empty"
tell (x:[]) = "one element: " ++ show x
tell (x:y:[]) = "two elements" ++ show x ++ "and" ++ show y
tell (x:y:z) = "many elements: " ++ show x ++ "and" ++ show y ++ "and etc"

initialF :: [[Char]] -> [[Char]]
initialF xs = [ all | all@('F':_) <- xs]

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". "  ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

{- initials' :: String -> String -> String -}
initials' firstname lastname = head [f|(f:_) <-firstname] : '.' : head [l|(l:_) <-lastname ] : ['.']

initials'' firstname lastname = head firstname: '.' : head lastname : "." 

calcBmis' :: [(Double,Double)] -> [Double]
calcBmis' xs = [ bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height^2

calcBmis'' :: [(Double,Double)] -> [String]
calcBmis'' xs = [ bmiTell(bmi w h) | (w, h) <- xs]
    where bmi weight height = weight / height^2

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs )

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _= []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

elem' :: Eq a => a -> [a] -> Bool 
elem' a [] = False
elem' a (x:xs) 
    | a == x    =True
    | otherwise =a `elem'` xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let  smallerOrEqual = [a | a <- xs, a<x]
         larger         = [a | a <-xs , a > x]
         other          = [a | a <-xs , a == x]
    in   quicksort smallerOrEqual ++[x] ++ quicksort larger
