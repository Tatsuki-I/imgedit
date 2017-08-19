module Main where

import System.Environment (getArgs)
import Imgedit

main :: IO ()
main = do print "hello"
          {-
          args <- getArgs
          file1 <- readFile $ args !! 0
          file2 <- readFile $ args !! 1
          --writeFile "" 
          writeFile (args !! 2) $ unlines (take 4 (lines file1))
                  ++ unlines (map (unwords . map show)
                                  (zipimg' (initImg $ lines file1)
                                           (initImg $ lines file2)))
                                           -}
