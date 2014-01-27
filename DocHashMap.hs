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

type Variants        = Map.HashMap Frame Format
type SubtreeVariants = Map.HashMap Doc   Variants

insertIfSuitable :: Int -> Format -> Variants -> Variants
insertIfSuitable w f | isSuitable w f = Map.insert (fmtToFrame f) f
                     | otherwise      = id

docToVariants :: Int -> Doc -> SubtreeVariants -> SubtreeVariants
docToVariants _ d stVar | isJust (Map.lookup d stVar) = stVar

docToVariants w d@(Text s) stVar = Map.insert d (mf $ s2fmt s) stVar where
    mf = \f -> if length s > w then Map.empty else Map.singleton (fmtToFrame f) f

docToVariants w d@(Indent i sd) stVar = (\m -> Map.insert d m sdVar) $
  fromMaybe Map.empty $ fmap indentF $ Map.lookup sd sdVar where
    sdVar   = docToVariants w sd stVar
    indentF = Map.foldl' (\m f -> insertIfSuitable w (indentFmt i f) m) Map.empty

docToVariants w d@(Beside ld rd) stVar = crossVariants w besideFmt d ld rd stVar
docToVariants w d@(Above  ld rd) stVar = crossVariants w  aboveFmt d ld rd stVar
docToVariants w d@(Choice ld rd) stVar = Map.insert d res rdVar where
    rdVar = docToVariants w rd $ docToVariants w ld stVar
    v     = fromMaybe Map.empty . flip Map.lookup rdVar
    res   = Map.unionWith min (v ld) (v rd)

crossVariants :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
crossVariants w f d ld rd stVar = (\m -> Map.insert d m rdVar) $
  Map.foldl' (\m lv -> Map.unionWith min m $
    Map.foldl' (\m rv -> insertIfSuitable w (f lv rv) m) Map.empty $ v rd
  ) Map.empty $ v ld where
    rdVar = docToVariants w rd $ docToVariants w ld stVar
    v     = fromMaybe Map.empty . flip Map.lookup rdVar

pretty :: Int -> Doc -> String
pretty w d = fromMaybe "Error" $ variants >>= (fmap (\f -> txtstr f 0 "")) . chooser where
    variants  = Map.lookup d $ docToVariants w d Map.empty
    chooser m = if Map.null m then Nothing else Just $ List.minimum $ Map.elems m

-- -------------------------------------------------------------------
-- Test --------------------------------------------------------------

heightToDoc :: [String] -> Int -> (Doc, [String])
heightToDoc (x:xs) 0 = (text x, xs)
heightToDoc (x:xs) n = (node >|< ((a >|< c) >//< (b >-< d)), zs) where
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
