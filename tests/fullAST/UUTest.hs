module UUTest where

import UU.Pretty
import Tree

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

heightToDoc :: Integer -> [String] -> (PP_Doc, [String])
heightToDoc h l = (treeToDoc tree, lTail)
  where
    (tree, lTail) = treeG l h
