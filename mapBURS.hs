{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MapBURS where

import Data.MemoTrie
import Data.Binary
import qualified Data.ByteString.Lazy as ByteLazy

import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as Map
--import qualified Data.Map as Map

import qualified Data.List as List

import System.Environment (getArgs)
import System.Random

import Format

-- -------------------------------------------------------------------
-- Formats -----------------------------------------------------------

type Variants = Map.HashMap Frame Format
type Doc = Int -> Variants

update :: Format -> Variants -> Variants
update fmt = Map.insertWith min (fmtToFrame fmt) fmt 

text :: String -> Doc
text s = \n ->
  let f = s2fmt s
  in if isSuitable n f
     then Map.singleton (fmtToFrame f) f
     else Map.empty

indent :: Int -> Doc -> Doc
indent i d = \n ->
  let docs = Map.elems (Map.filter (isSuitable (n-i)) (d n))
      vs'  = map ((\f -> (fmtToFrame f, f)) . indentFmt i) docs
  in Map.fromList vs'

choice, beside, above :: Doc -> Doc -> Doc
choice a b = \n -> Map.foldl' (flip update) (b n) (a n)
beside a b = \n -> cross n besideFmt (a n) (b n)
above  a b = \n -> cross n aboveFmt  (a n) (b n)

cross :: Int -> (Format -> Format -> Format) -> Variants -> Variants -> Variants
cross n f a b = 
 Map.foldl' (\m f1 -> Map.foldl' (flip $ g . (f f1)) m b) Map.empty a where
  g f = if isSuitable n f then update f else id

(>//<), (>|<), (>-<) :: Doc -> Doc -> Doc
(>//<) = choice
(>|<)  = beside
(>-<)  = above 

pretty :: Int -> Doc -> String
pretty n d = 
 case Map.elems (d n) of
  [] -> error "No layout"
  xs -> (\x -> txtstr x 0 "") $ List.minimum xs

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------
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

{-
main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width
-}
