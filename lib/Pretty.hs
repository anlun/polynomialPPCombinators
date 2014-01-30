module Pretty where

import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List

import Format
import Doc
import Variants

type SubtreeVariants = Map.HashMap Doc Variants

docToVar :: Int -> Doc -> SubtreeVariants -> SubtreeVariants
docToVar _ d var | isJust (Map.lookup d var) = var

docToVar w d@(Text s) var = Map.insert d (checkUpdate w (s2fmt s) Map.empty) var where

docToVar w d@(Indent i sd) var = (\m -> Map.insert d m sdVar) $
  fromMaybe Map.empty $ fmap indentF $ Map.lookup sd sdVar where
    sdVar   = docToVar w sd var
    indentF = Map.foldl' (\m f -> checkUpdate w (indentFmt i f) m) Map.empty

docToVar w d@(Beside ld rd) var = cross w besideFmt d ld rd var
docToVar w d@(Above  ld rd) var = cross w  aboveFmt d ld rd var
docToVar w d@(Choice ld rd) var = Map.insert d res rdVar where
    rdVar = docToVar w rd $ docToVar w ld var
    v     = fromMaybe Map.empty . flip Map.lookup rdVar
    res   = Map.unionWith min (v ld) (v rd)

cross :: Int -> (Format -> Format -> Format) -> Doc -> Doc -> Doc -> SubtreeVariants -> SubtreeVariants
cross w f d ld rd var = (\m -> Map.insert d m rdVar) $
  Map.foldl' (\m lv -> Map.unionWith min m $
    Map.foldl' (\m rv -> checkUpdate w (f lv rv) m) Map.empty rVars
  ) Map.empty $ v ld where
    rdVar = docToVar w rd $ docToVar w ld var
    v     = fromMaybe Map.empty . flip Map.lookup rdVar
    rVars = v rd

pretty :: Int -> Doc -> String
pretty w d = fromMaybe "Error" $ variants >>= (fmap (\f -> txtstr f 0 "")) . chooser where
    variants  = Map.lookup d $ docToVar w d Map.empty
    chooser m = if Map.null m then Nothing else Just $ List.minimum $ Map.elems m
