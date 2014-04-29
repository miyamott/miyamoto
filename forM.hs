import Prelude.Monad
import Control.Monad
 
main =do
    colors <- forM [1,2,3,4] $ \a -> do
        putstrLn $ "which color do you associate with the numbert "
                 ++ show a ++ "?"
        color <- getLine
        return color
    putstrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
