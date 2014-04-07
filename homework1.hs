factorial'gard :: Integer -> Integer  
factorial'gard n 
    | n==0      = 1
    | otherwise = n * factorial'gard (n-1)

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
