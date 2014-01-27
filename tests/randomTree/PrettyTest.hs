module PrettyTest where

import Doc

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs) where
    node    = text x
    f       = flip heightToDoc (n-1)     
    (a, ys) = f xs 
    (b, zs) = f ys 
    (c, as) = f zs 
    (d, bs) = f as
