module Main where
import System.IO
import System.Environment (getArgs)
import qualified GeneralizedContext as GC


main :: IO () 
main = do args <- getArgs
          case args !! 0 of
               "solveBench"         -> GC.solveFile       (read (args !! 1) :: Int)
               "solveInput"         -> GC.solveInput      (read (args !! 1) :: Int)
               "translateBench"     -> GC.translateInput  (read (args !! 1) :: Int)  
               _                    -> putStrLn "please enter other commands"










          
          
