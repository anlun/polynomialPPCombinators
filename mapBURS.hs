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

-- -------------------------------------------------------------------
-- Formatting one layout ---------------------------------------------
-- From Swierstra UU Lib ---------------------------------------------

spaces = ' ':spaces
sp n = if n >= 0 then take n spaces else ""

data Format = Elem { height  :: Int
                   , last_w  :: Int
                   , total_w :: Int
                   , txtstr  :: Int -> String -> String
                   }

instance Eq Format  where
  x == y =  height  x == height  y
         && total_w x == total_w y
         && last_w  x == last_w  y

instance Ord Format where
  x < y =   height x <  height y
        || (height x == height y && total_w x < total_w y)
  min x y = if x < y then x else y

s2fmt     :: String -> Format
s2fmt s   = Elem 1 l l (\_ -> (s++))
  where l = length s

indentFmt :: Int -> Format -> Format
indentFmt i   (Elem dh dl dw dt)
   = Elem dh (i + dl) (i + dw) (\n -> ((sp i) ++) . dt (i + n))

aboveFmt, besideFmt :: Format -> Format -> Format
(Elem uh ul uw ut) `aboveFmt` (Elem lh ll lw lt)
  = Elem (uh + lh) ll (uw `max` lw)
         (makeTsAbove ut lt)
  where makeTsAbove ut lt = \n -> let nl_skip = (('\n':sp n)++)
                                  in  ut n . nl_skip . lt n
(Elem lh ll lw lt) `besideFmt` (Elem rh rl rw rt)
  = Elem (lh + rh - 1) (ll + rl)
         (lw `max` (ll + rw)) (\n -> lt n . rt (ll + n))

isSuitable :: Int -> Format -> Bool
isSuitable width fmt = (total_w fmt) <= width

-- -------------------------------------------------------------------
-- Frame -------------------------------------------------------------
-- From Swierstra UU Lib ---------------------------------------------

data Frame = F Int Int
             deriving (Eq, Generic)

instance Hashable Frame
instance Ord Frame where
  max x@(F w lw) y@(F w' lw')
    | w >  w'             = x
    | w == w' && lw > lw' = x
    | otherwise           = y

fmtToFrame :: Format -> Frame
fmtToFrame fmt = F (total_w fmt) (last_w fmt)

isFrameSuitable :: Int -> Frame -> Bool
isFrameSuitable n (F w lw) = w <= n

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
