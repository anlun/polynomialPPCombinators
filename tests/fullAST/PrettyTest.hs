module PrettyTest where

import System.Environment (getArgs)
import System.Random

import Doc
import Pretty

data TreeR = T String
           | Node TreeR TreeR

treeToDoc :: TreeR -> Doc
treeToDoc (T s)        = text s
treeToDoc (Node lt rt) = besideVariant >//< aboveVariant
  where
    p   = text "-"
    lft = treeToDoc lt
    rft = treeToDoc rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft


treeG :: [Char] -> Integer -> (TreeR, [Char])
treeG (x:xs) 0 = (T (x:""), xs)
treeG l n = (Node t1 t2, l2)
  where
    (t1, l1) = treeG l  (n-1)
    (t2, l2) = treeG l1 (n-1)

heightToDoc :: Integer -> [Char] -> Doc
heightToDoc h l = treeToDoc $ fst $ treeG l h
