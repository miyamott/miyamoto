lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

firstLetter :: String -> String
firstLetter "" = " Empty"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ "is "++[x]

bmiTell :: Double -> Double -> String
bmiTell weight height
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