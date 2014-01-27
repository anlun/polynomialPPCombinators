module AltPrettyTest where

import System.Environment (getArgs)
import System.Random

import AltPretty

data TreeR = TR String
           | NodeR TreeR TreeR

treeToDocR :: TreeR -> Doc
treeToDocR (TR s)        = text s
treeToDocR (NodeR lt rt) = besideVariant >//< aboveVariant
  where
    p   = text "-"
    lft = treeToDocR lt
    rft = treeToDocR rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft


treeRG :: [Char] -> Integer -> (TreeR, [Char])
treeRG (x:xs) 0 = (TR (x:""), xs)
treeRG l n = (NodeR t1 t2, l2)
  where
    (t1, l1) = treeRG l  (n-1)
    (t2, l2) = treeRG l1 (n-1)

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs)
  where
    node    = text x
    (a, ys) = heightToDoc xs (n-1)
    (b, zs) = heightToDoc ys (n-1)
    (c, as) = heightToDoc zs (n-1)
    (d, bs) = heightToDoc as (n-1)

bestR :: Int -> Int -> IO ()
bestR tHeight width =
  do
    g <- getStdGen
--    putStrLn $ pretty width (treeToDocR . fst $ treeRG (randomRs ('a', 'z') g) tHeight)
    putStrLn $ pretty width $ fst (heightToDoc (map (\x -> x:"") (randomRs ('a', 'z') g)) tHeight)

main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width
