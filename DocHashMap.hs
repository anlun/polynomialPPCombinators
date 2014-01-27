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
import Doc

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
docToVariants _ d stVar | isJust (Map.lookup d stVar) = stVar

docToVariants width d@(Text s) stVar | length s > width = Map.insert d Map.empty stVar
docToVariants _     d@(Text s) stVar = Map.insert d (Map.singleton fr fmt) stVar where
    fmt = s2fmt s
    fr  = fmtToFrame fmt

docToVariants width d@(Indent i subDoc) stVar =  
  g $ fromMaybe Map.empty $ fmap indented subDocVariants where
    g                  = \m -> Map.insert d m variantsWithSubDoc
    subDocVariants     = Map.lookup subDoc variantsWithSubDoc
    variantsWithSubDoc = docToVariants width subDoc stVar
    indented           = Map.foldl' (\m v -> let iv = indentFmt i v in
                                       insertIfSuitable width iv m
                                    ) Map.empty

docToVariants width d@(Beside ld rd) stVar = crossVariants width besideFmt d ld rd stVar
docToVariants width d@(Above  ld rd) stVar = crossVariants width aboveFmt  d ld rd stVar
docToVariants width d@(Choice ld rd) stVar = Map.insert d docResult varWithRD where
    varWithLD = docToVariants width ld stVar
    l         = fromMaybe Map.empty $ Map.lookup ld varWithLD
    varWithRD = docToVariants width rd varWithLD
    r         = fromMaybe Map.empty $ Map.lookup rd varWithRD
    docResult = Map.unionWith min l r

crossVariants :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
crossVariants width f d leftDoc rightDoc stVar =
  g $ fromMaybe Map.empty $ leftVariants >>= (flip fmap rightVariants) . crossed where
    g                    = \m -> Map.insert d m variantsWithRightDoc
    variantsWithLeftDoc  = docToVariants width leftDoc  stVar
    leftVariants         = Map.lookup leftDoc  variantsWithLeftDoc
    variantsWithRightDoc = docToVariants width rightDoc variantsWithLeftDoc
    rightVariants        = Map.lookup rightDoc variantsWithRightDoc
    crossed              = variantCross width f

variantCross :: Int -> (Format -> Format -> Format) -> Variants -> Variants -> Variants
variantCross width f lvs rvs = Map.foldl' (\m lv -> Map.unionWith min m $
    Map.foldl' (\m rv -> insertIfSuitable width (f lv rv) m) Map.empty rvs
  ) Map.empty lvs

-- -------------------------------------------------------------------
-- Pretty ------------------------------------------------------------

pretty :: Int -> Doc -> String
pretty width d =
  fromMaybe "Error" $ variants >>= (fmap (\f -> txtstr f 0 "")) . chooser
  where
    allVariants = docToVariants width d Map.empty
    variants    = Map.lookup d allVariants
    chooser     = Map.foldl' (\m v -> Just $ min (fromMaybe v m) v) Nothing

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs)
  where
    node    = text x
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
