module Main where

import System.Environment (getArgs)
import Imgedit

main :: IO ()
main = do args <- getArgs
          file1 <- readFile $ args !! 0
          file2 <- readFile $ args !! 1
          writeFile "test2-export.ppm" $ exportImg $ blend Add (initImg (args !! 0) file1) (initImg (args !! 1) file2)
