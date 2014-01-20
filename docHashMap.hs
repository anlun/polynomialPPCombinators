{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
--import qualified Data.Map as Map

import System.Environment (getArgs)
import System.Random

-- -------------------------------------------------------------------
-- Formatting one layout ---------------------------------------------
-- From Swierstra UU Lib ---------------------------------------------

spaces = ' ':spaces
sp n = if n >= 0 then take n spaces else ""

type T_PW  = Int
type T_PLL = Int
type T_PH  = Int

data Format = Elem { height  :: T_PH
                   , last_w  :: T_PLL
                   , total_w :: T_PW
                   , txtstr  :: Int -> String -> String
                   }

instance Eq Format  where
  x == y =  height  x == height  y
         && total_w x == total_w y
         && last_w  x == last_w  y

instance Ord Format where
  x <  y =  height x <  height y
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

data Frame = F T_PW T_PLL
             deriving (Eq, Generic)

instance Hashable Frame
instance Ord Frame where
  max x@(F w lw) y@(F w' lw')
    | w >  w'             = x
    | w == w' && lw > lw' = x
    | otherwise           = y

fmtToFrame :: Format -> Frame
fmtToFrame fmt = F (total_w fmt) (last_w fmt)

-- -------------------------------------------------------------------
-- Formats -----------------------------------------------------------

type Variants = Map.HashMap Frame Format
--type Variants = Map.Map Frame Format
update :: Format -> Variants -> Variants
update fmt = Map.insertWith min (fmtToFrame fmt) fmt 

-- -------------------------------------------------------------------
-- Subtree Variants --------------------------------------------------

--type SubtreeVariants = Map.Map Doc Variants
type SubtreeVariants = Map.HashMap Doc Variants

docToVariants :: Int -> Doc -> SubtreeVariants -> SubtreeVariants
docToVariants maxWidth doc startVariants | isJust (Map.lookup doc startVariants) = startVariants

docToVariants maxWidth d@(Text s) startVariants
  | length s > maxWidth = Map.insert d Map.empty startVariants
docToVariants maxWidth d@(Text s) startVariants
  = Map.insert d (Map.singleton fr fmt) startVariants
  where
    fmt = s2fmt s
    fr  = fmtToFrame fmt

docToVariants maxWidth d@(Indent i subDoc) startVariants =  
  if (isNothing subDocVariants)
    then Map.insert d Map.empty variantsWithSubDoc
    else Map.insert d indented  variantsWithSubDoc
  where
    subDocVariants     = Map.lookup subDoc variantsWithSubDoc
    variantsWithSubDoc = docToVariants maxWidth subDoc startVariants
    indented =
      Map.foldl'
      (\m v -> 
        let indentedV = indentFmt i v in
        if (isSuitable maxWidth indentedV)
          then Map.insert (fmtToFrame indentedV) indentedV m
          else m
      )
      Map.empty
      (fromJust subDocVariants)

docToVariants maxWidth d@(Beside leftDoc rightDoc) startVariants =
  crossVariants maxWidth besideFmt d leftDoc rightDoc startVariants
docToVariants maxWidth d@(Above  leftDoc rightDoc) startVariants =
  crossVariants maxWidth aboveFmt  d leftDoc rightDoc startVariants

docToVariants maxWidth d@(Choice leftDoc rightDoc) startVariants =
  Map.insert d docResult variantsWithRightDoc
  where
    variantsWithLeftDoc  = docToVariants maxWidth leftDoc  startVariants
    leftVariants  = Map.lookup leftDoc  variantsWithLeftDoc
    variantsWithRightDoc = docToVariants maxWidth rightDoc variantsWithLeftDoc
    rightVariants = Map.lookup rightDoc variantsWithRightDoc

    --docResult = Map.unionWith min l r
    l = fromMaybe Map.empty leftVariants
    lList = Map.toList l
    r = fromMaybe Map.empty rightVariants
    rList = Map.toList r

    docResult = Map.fromList $ lList ++ rList

crossVariants :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
crossVariants maxWidth f d leftDoc rightDoc startVariants =
  if (isNothing leftVariants || isNothing rightVariants)
    then Map.insert d Map.empty variantsWithRightDoc
    else Map.insert d crossed   variantsWithRightDoc
  where
    variantsWithLeftDoc  = docToVariants maxWidth leftDoc  startVariants
    leftVariants  = Map.lookup leftDoc  variantsWithLeftDoc
    variantsWithRightDoc = docToVariants maxWidth rightDoc variantsWithLeftDoc
    rightVariants = Map.lookup rightDoc variantsWithRightDoc

    crossed = variantCross maxWidth f (fromJust leftVariants) (fromJust rightVariants)

variantCross :: Int -> (Format -> Format -> Format) -> Variants -> Variants -> Variants
variantCross maxWidth f leftVariants rightVariants = 
  Map.foldl'
  (\m lv ->
    let {fMap =
      Map.foldl'
      (\m rv ->
        let fV = f lv rv in
        if (isSuitable maxWidth fV)
          then Map.insert (fmtToFrame fV) fV m
          else m
      )
      Map.empty
      rightVariants}
    in
    Map.unionWith min fMap m
  )
  Map.empty
  leftVariants

-- -------------------------------------------------------------------
-- Doc ---------------------------------------------------------------

data Doc = Text String
         | Indent Int Doc
         | Doc `Beside` Doc
         | Doc `Above`  Doc
         | Doc `Choice` Doc
         deriving (Eq, Ord, Show, Generic)

instance Hashable Doc {-where
  hash (Text       s) = hash s
  hash (Indent i   d) = hash (i, d)
  hash (Beside d1 d2) = hash (d1, d2, 0)
  hash (Above  d1 d2) = hash (d1, d2, 1)
  hash (Choice d1 d2) = hash (d1, d2, 2)
-}

(>|<) :: Doc -> Doc -> Doc
a >|< b = Beside a b

(>-<) :: Doc -> Doc -> Doc
a >-< b = Above a b

(>//<) :: Doc -> Doc -> Doc
a >//< b = Choice a b

-- -------------------------------------------------------------------
-- Pretty ------------------------------------------------------------

pretty :: Int -> Doc -> String
pretty width d =
  case variants of
    Nothing   -> "Error"
    otherwise -> fromMaybe "Error" (result >>= (\f -> return $ txtstr f 0 "" ))
  where
    subtreeVariants = docToVariants width d Map.empty
    variants = Map.lookup d subtreeVariants
    result =
      Map.foldl'
      (\m v -> Just $ min (fromMaybe v m) v)
      --(\m v -> Just $ min (s2fmt "*") (s2fmt "**"))
      Nothing
      (fromJust variants)

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------

data Tree = T
          | Node Tree Tree

treeToDoc :: Tree -> Doc
treeToDoc  T           = Text "*"
treeToDoc (Node lt rt) = besideVariant >//< aboveVariant
  where
    p   = Text "-"
    lft = treeToDoc lt
    rft = treeToDoc rt
    besideVariant = p >|< lft >|< rft
    aboveVariant  = p >-< lft >-< rft

exTreeG :: Integer -> Tree
exTreeG 0 = T
exTreeG n = Node subtree subtree
  where
    subtree = exTreeG $ n - 1

data TreeR = TR String
           | NodeR TreeR TreeR

treeToDocR :: TreeR -> Doc
treeToDocR (TR s)        = Text s
treeToDocR (NodeR lt rt) = besideVariant >//< aboveVariant
  where
    p   = Text "-"
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

best tHeight width =
    do
      --(putStrLn . show) tree
      putStrLn $ pretty width tree
    where
      tree = treeToDoc $ exTreeG tHeight

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (Text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs)
  where
    node    = Text x
    (a, ys) = heightToDoc xs (n-1)
    (b, zs) = heightToDoc ys (n-1)
    (c, as) = heightToDoc zs (n-1)
    (d, bs) = heightToDoc as (n-1)

bestR tHeight width =
  do
    g <- getStdGen
    --putStrLn $ pretty width (treeToDocR . fst $ treeRG (randomRs ('a', 'z') g) tHeight)
    putStrLn $ pretty width $ fst (heightToDoc (map (\x -> x:"") (randomRs ('a', 'z') g)) tHeight)

main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width
