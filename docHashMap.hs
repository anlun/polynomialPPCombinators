{-# LANGUAGE DeriveGeneric #-}

--module DocHashMap where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List

import System.Environment (getArgs)
import System.Random

import Format

-- -------------------------------------------------------------------
-- Formats -----------------------------------------------------------

type Variants = Map.HashMap Frame Format
update :: Format -> Variants -> Variants
update fmt = Map.insertWith min (fmtToFrame fmt) fmt 

insertIfSuitable :: Int -> Format -> Variants -> Variants
insertIfSuitable w fmt m = if (isSuitable w fmt)
                           then Map.insert (fmtToFrame fmt) fmt m
                           else m

-- -------------------------------------------------------------------
-- Subtree Variants --------------------------------------------------

type SubtreeVariants = Map.HashMap Doc Variants

docToVariants :: Int -> Doc -> SubtreeVariants -> SubtreeVariants
docToVariants width d startVariants | isJust (Map.lookup d startVariants) = startVariants

docToVariants width d@(Text s) startVariants
  | length s > width = Map.insert d Map.empty startVariants
docToVariants width d@(Text s) startVariants
  = Map.insert d (Map.singleton fr fmt) startVariants
  where
    fmt = s2fmt s
    fr  = fmtToFrame fmt

docToVariants width d@(Indent i subDoc) startVariants =  
  g $ fromMaybe Map.empty $ subDocVariants >>= return . indented
  where
    g                  = \m -> Map.insert d m variantsWithSubDoc
    subDocVariants     = Map.lookup subDoc variantsWithSubDoc
    variantsWithSubDoc = docToVariants width subDoc startVariants
    indented           = Map.foldl' (\m v -> let iv = indentFmt i v in
                                       insertIfSuitable width iv m
                                    ) Map.empty

docToVariants width d@(Beside ld rd) startVariants =
  crossVariants width besideFmt d ld rd startVariants
docToVariants width d@(Above  ld rd) startVariants =
  crossVariants width aboveFmt  d ld rd startVariants

docToVariants width d@(Choice ld rd) startVariants =
  Map.insert d docResult varWithRD
  where
    varWithLD = docToVariants width ld startVariants
    l         = fromMaybe Map.empty $ Map.lookup ld varWithLD
    varWithRD = docToVariants width rd varWithLD
    r         = fromMaybe Map.empty $ Map.lookup rd varWithRD
    docResult = Map.unionWith min l r

crossVariants :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
crossVariants width f d leftDoc rightDoc startVariants =
  g $ fromMaybe Map.empty $ (>>=) leftVariants $ \l -> (>>=) rightVariants $ return . (crossed l)
  where
    g                    = \m -> Map.insert d m variantsWithRightDoc
    variantsWithLeftDoc  = docToVariants width leftDoc  startVariants
    leftVariants         = Map.lookup leftDoc  variantsWithLeftDoc
    variantsWithRightDoc = docToVariants width rightDoc variantsWithLeftDoc
    rightVariants        = Map.lookup rightDoc variantsWithRightDoc
    crossed              = variantCross width f

variantCross :: Int -> (Format -> Format -> Format) -> Variants -> Variants -> Variants
variantCross width f leftVariants rightVariants = Map.foldl' (\m lv ->
    let fMap = Map.foldl' (\m rv -> let fV = f lv rv in
                             insertIfSuitable width fV m
                          ) Map.empty rightVariants in
    Map.unionWith min fMap m
  ) Map.empty leftVariants

-- -------------------------------------------------------------------
-- Doc ---------------------------------------------------------------

data Doc = Text String
         | Indent Int Doc
         | Doc `Beside` Doc
         | Doc `Above`  Doc
         | Doc `Choice` Doc
         deriving (Eq, Ord, Show, Generic)
instance Hashable Doc

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
  fromMaybe "Error" $ variants >>= chooser >>= return . (\f -> txtstr f 0 "")
  where
    allVariants = docToVariants width d Map.empty
    variants    = Map.lookup d allVariants
    chooser     = Map.foldl' (\m v -> Just $ min (fromMaybe v m) v) Nothing

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (Text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs)
  where
    node    = Text x
    f       = flip heightToDoc (n-1)     
    (a, ys) = f xs 
    (b, zs) = f ys 
    (c, as) = f zs 
    (d, bs) = f as 

bestR tHeight width =
  do
    g <- getStdGen
    putStrLn $ pretty width $ fst $ heightToDoc (randStrList g) tHeight
  where
    randStrList = (map (:"")) . (randomRs ('a', 'z'))

main = do
  args <- getArgs
  let tHeight = read (head args); width = read (args !! 1) in bestR tHeight width
