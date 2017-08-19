module Imgedit where

import Data.List


type Pixel = (Red, Green, Blue)
type Row = [Pixel]
type Red = Int
type Green = Int
type Blue = Int
data PPM = PPM
         { _path :: FilePath
         , _mode :: Word
         , _size :: (Int, Int)
         , _max :: Int
         , _img :: [Row]
         } deriving (Show)

initImg           :: FilePath -> String -> PPM
initImg path file =  PPM { _path = path
                         , _mode = (read . tail . head . lines) file
                         , _size = s
                         , _max = (read . (!! 3) . lines) file
                         , _img = (map (map to3tuple) . toLsLs row . toLsLs 3 . toIntLs . drop 4 . lines) file }
                     where s@(row, column) = (to2tuple . map read . words . (!! 2) . lines) file

exportImg     :: PPM -> String
exportImg ppm =  "P" ++ show (_mode ppm) ++ "\n"
                 ++ "# CREATOR: Imgedit\n"
                 ++ (unwords . map show . twoTupleToLs) (_size ppm) ++ "\n"
                 ++ show (_max ppm) ++ "\n"
                 ++ (unlines . map (unlines . map (unwords . map show . threeTupleToLs))) (_img ppm)
                 -- ++ (unlines . map unwords . map show . threeTupleToLs) (_img ppm) ++ "\n"

to3tuple :: [a] -> (a, a, a)
to3tuple l =  (head l, (head . tail) l, (head . tail . tail) l)

to2tuple :: [a] -> (a, a)
to2tuple l =  (head l, (head . tail) l)

twoTupleToLs :: (a, a) -> [a]
twoTupleToLs (f, s) =  [f, s]

threeTupleToLs :: (a, a, a) -> [a]
threeTupleToLs (f, s, t) =  [f, s, t]

toIntLs :: [String] -> [Int]
toIntLs =  map read

toLsLs    :: Int -> [a] -> [[a]]
toLsLs ln =  unfoldr (f ln)
             where f       :: Int -> [a] -> Maybe ([a], [a])
                   f _  [] =  Nothing
                   f ln l  =  Just (splitAt ln l)
