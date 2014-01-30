module UUTest where

import UU.Pretty

heightToDoc :: [String] -> Int -> (PP_Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< (join $ (a >|< c) >//< (b >-< d)), zs) where
    node    = text "-"
    f       = flip heightToDoc (n-1)
    (a, ys) = f xs 
    (b, zs) = f ys 
    (c, as) = f zs 
    (d, bs) = f as
