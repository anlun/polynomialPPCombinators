module PrettyTest where

import System.Environment (getArgs)
import System.Random

import Doc
import Pretty
import Format

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs) where
    node    = text x
    f       = flip heightToDoc (n-1)     
    (a, ys) = f xs 
    (b, zs) = f ys 
    (c, as) = f zs 
    (d, bs) = f as 

bestR tHeight width =
  do
    g <- getStdGen
    putStrLn $ pretty width $ fst $ heightToDoc (randStrList g) tHeight
  where
    randStrList = (map (:"")) . (randomRs ('a', 'z'))

main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width
