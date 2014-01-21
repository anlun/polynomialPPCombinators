module ClearUU where

import UU.Pretty
import System.Environment (getArgs)
import System.Random

data Tree = T String
          | Node Tree Tree

treeToDoc :: Tree -> PP_Doc
treeToDoc (T s)        = text s
treeToDoc (Node lt rt) = join $ besideVariant >//< aboveVariant
  where
    p   = text "-"
    lft = treeToDoc lt
    rft = treeToDoc rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft

treeToDoc_1 :: Tree -> PP_Doc
treeToDoc_1 (T s)        = text s
treeToDoc_1 (Node lt rt) =
  join $ (par >>|<< par >>|<< par) >>//<< (par >>-<< par >>-<< par) >>$< [p, lft, rft]
  where
    p = text "-"
    lft = treeToDoc_1 lt
    rft = treeToDoc_1 rt    


exTreeG :: Integer -> Tree
exTreeG 0 = T "a"
exTreeG n = Node subtree subtree
  where
    subtree = exTreeG $ n - 1

-- Умирает при best 8 ...
-- Даже на best 8 1000000

heightToDoc :: [String] -> Int -> (PP_Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< (join $ (a >|< c) >//< (b >-< d)), zs)
  where
    node    = text x
    (a, ys) = heightToDoc xs (n-1)
    (b, zs) = heightToDoc ys (n-1)
    (c, as) = heightToDoc zs (n-1)
    (d, bs) = heightToDoc as (n-1)

bestR tHeight width =
  do
    g <- getStdGen
    --putStrLn $ pretty width (treeToDocR . fst $ treeRG (randomRs ('a', 'z') g) tHeight)
    flip render width $ fst (heightToDoc (map (\x -> x:"") (randomRs ('a', 'z') g)) tHeight)

--main = do
--  args <- getArgs
--  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width

--best tHeight width =
--  do
--    --render (treeToDoc $ exTreeG tHeight) width
--    render (treeToDoc_1 $ exTreeG tHeight) width
--    putStrLn ""

--main = do
--  args <- getArgs
--  let tHeight = read (head args); width = read (args !! 1) in best tHeight width
