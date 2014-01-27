module UUTest where

import UU.Pretty

heightToDoc :: [String] -> Int -> (PP_Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< (join $ (a >|< c) >//< (b >-< d)), zs)
  where
    node    = text x
    (a, ys) = heightToDoc xs (n-1)
    (b, zs) = heightToDoc ys (n-1)
    (c, as) = heightToDoc zs (n-1)
    (d, bs) = heightToDoc as (n-1)
