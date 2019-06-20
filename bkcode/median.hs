import System.Environment (getArgs)
import Lda1
main :: IO ()
main = do
   values <-  getArgs
   print . median $ map read values
