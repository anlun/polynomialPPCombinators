module UUTest where

import UU.Pretty
import System.Environment (getArgs)
import System.Random

data Tree = T String
          | Node Tree Tree

treeG :: [Char] -> Integer -> (Tree, [Char])
treeG (x:xs) 0 = (T (x:""), xs)
treeG l n = (Node t1 t2, l2)
  where
    (t1, l1) = treeG l  (n-1)
    (t2, l2) = treeG l1 (n-1)

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

heightToDoc :: Integer -> [Char] -> PP_Doc
heightToDoc h l = treeToDoc $ fst $ treeG l h
