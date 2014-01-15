{-# LANGUAGE DeriveGeneric #-}

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
data Doc      = D Int Variants

update :: Format -> Variants -> Variants
update fmt = Map.insertWith min (fmtToFrame fmt) fmt 

text :: Int -> String -> Doc
text n s = D n vs where
 f  = s2fmt s
 vs = if isSuitable n f
      then Map.singleton (fmtToFrame f) f
      else Map.empty

indent :: Int -> Int -> Doc -> Doc
indent n i (D _ vs) = D n (Map.fromList vs') where
 docs = Map.elems (Map.filter (isSuitable (n-i)) vs)
 vs'  = map ((\f -> (fmtToFrame f, f)) . indentFmt i) docs

choice, beside, above :: Int -> Doc -> Doc -> Doc
choice n (D _ a) (D _ b) = D n $ Map.foldl' (flip update) b a
beside n (D _ a) (D _ b) = D n $ cross n besideFmt a b
above  n (D _ a) (D _ b) = D n $ cross n aboveFmt  a b

cross :: Int -> (Format -> Format -> Format) -> Variants -> Variants -> Variants
cross n f a b = Map.foldl' (\m f1 -> Map.foldl' (flip $ g . (f f1)) m b) Map.empty a where
 g f = if isSuitable n f then update f else id

(>//<), (>|<), (>-<) :: Doc -> Doc -> Doc
(>//<) a@(D n _) = choice n a
(>|<)  a@(D n _) = beside n a
(>-<)  a@(D n _) = above  n a

pretty :: Int -> Doc -> String
pretty n (D _ vs) = 
 case Map.elems vs of
  [] -> error "Что-то написать"
  xs -> (\x -> txtstr x 0 "") $ List.minimum xs

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------
data TreeR = TR String
           | NodeR TreeR TreeR

treeToDocR :: Int -> TreeR -> Doc
treeToDocR n (TR s)        = text n s
treeToDocR n (NodeR lt rt) = besideVariant >//< aboveVariant
  where
    p   = text n "-"
    lft = treeToDocR n lt
    rft = treeToDocR n rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft


treeRG :: [Char] -> Integer -> (TreeR, [Char])
treeRG (x:xs) 0 = (TR (x:""), xs)
treeRG l n = (NodeR t1 t2, l2)
  where
    (t1, l1) = treeRG l  (n-1)
    (t2, l2) = treeRG l1 (n-1)

bestR :: Integer -> Int -> IO ()
bestR tHeight width =
  do
    g <- getStdGen
    putStrLn $ pretty width ((treeToDocR width) . fst $ treeRG (randomRs ('a', 'z') g) tHeight)

main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width