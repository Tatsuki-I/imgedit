module Imgedit where

import Data.List


type Pixel = (Int, Int, Int)
type Row = [Pixel]
type Red = Int
type Green = Int
type Blue = Int
data BlendMode = Add
               | Sub
               | Mul
               | Screen
               | Burn
               | Overlay
                 deriving (Show)
data PPM = PPM
         { _path :: FilePath
         , _mode :: Word
         , _size :: (Int, Int)
         , _max  :: Int
         , _img  :: [Row]
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

blendMap         :: (Int -> Int -> Int) -> Pixel -> Pixel -> Pixel
blendMap f p1 p2 =  to3tuple $ zipWith (g) (threeTupleToLs p1) (threeTupleToLs p2)
                    where g     :: Int -> Int -> Int
                          g x y =  if f x y < 0
                                     then 0
                                     else if f x y > 255
                                            then 255
                                            else f x y

blend         :: BlendMode -> PPM -> PPM -> PPM
blend m p1 p2 =  case m of
                   Add -> p1 { _img = zipWith (zipWith (blendMap (+))) (_img p1) (_img p2) }
                   Sub -> p1 { _img = zipWith (zipWith (blendMap (-))) (_img p1) (_img p2) }


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
