{-# LANGUAGE DeriveGeneric #-}

module Pretty where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List

import Format
import Doc

type Variants        = Map.HashMap Frame Format
type SubtreeVariants = Map.HashMap Doc   Variants

insertIfSuitable :: Int -> Format -> Variants -> Variants
insertIfSuitable w f | isSuitable w f = Map.insert (fmtToFrame f) f
                     | otherwise      = id

docToVariants :: Int -> Doc -> SubtreeVariants -> SubtreeVariants
docToVariants _ d var | isJust (Map.lookup d var) = var

docToVariants w d@(Text s) var = Map.insert d (mf $ s2fmt s) var where
    mf = \f -> if length s > w then Map.empty else Map.singleton (fmtToFrame f) f

docToVariants w d@(Indent i sd) var = (\m -> Map.insert d m sdVar) $
  fromMaybe Map.empty $ fmap indentF $ Map.lookup sd sdVar where
    sdVar   = docToVariants w sd var
    indentF = Map.foldl' (\m f -> insertIfSuitable w (indentFmt i f) m) Map.empty

docToVariants w d@(Beside ld rd) var = crossVariants w besideFmt d ld rd var
docToVariants w d@(Above  ld rd) var = crossVariants w  aboveFmt d ld rd var
docToVariants w d@(Choice ld rd) var = Map.insert d res rdVar where
    rdVar = docToVariants w rd $ docToVariants w ld var
    v     = fromMaybe Map.empty . flip Map.lookup rdVar
    res   = Map.unionWith min (v ld) (v rd)

docToVariants w d@(OneLine sd) var = (\m -> Map.insert d m sdVar) $
  fromMaybe Map.empty $ fmap filterOneLiners $ Map.lookup sd sdVar where
    sdVar           = docToVariants w sd var
    filterOneLiners = Map.filter $ (<= 1) . height

crossVariants :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
crossVariants w f d ld rd var = (\m -> Map.insert d m rdVar) $
  Map.foldl' (\m lv -> Map.unionWith min m $
    Map.foldl' (\m rv -> insertIfSuitable w (f lv rv) m) Map.empty $ v rd
  ) Map.empty $ v ld where
    rdVar = docToVariants w rd $ docToVariants w ld var
    v     = fromMaybe Map.empty . flip Map.lookup rdVar

pretty :: Int -> Doc -> String
pretty w d = fromMaybe "Error" $ variants >>= (fmap (\f -> txtstr f 0 "")) . chooser where
    variants  = Map.lookup d $ docToVariants w d Map.empty
    chooser m = if Map.null m then Nothing else Just $ List.minimum $ Map.elems m
