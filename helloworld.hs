
import Control.Monad

main :: IO [()]
main = do
     colors <- forM [1,2,3,4] $ \a -> do
           putStrLn $ "which color do you associate with th number "
                       ++ show a ++ "?"
           color <- getLine
           return color
     putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
     mapM putStrLn colors
