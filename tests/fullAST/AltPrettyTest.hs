module AltPrettyTest where

import AltPretty
import Tree

treeToDoc :: Tree -> Doc
treeToDoc (T s)        = text s
treeToDoc (Node lt rt) = besideVariant >//< aboveVariant
  where
    p   = text "-"
    lft = treeToDoc lt
    rft = treeToDoc rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft

heightToDoc :: Integer -> [String] -> (Doc, [String])
heightToDoc h l = (treeToDoc tree, lTail)
  where
    (tree, lTail) = treeG l h
