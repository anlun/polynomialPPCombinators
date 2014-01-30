module AltPretty where

--import Data.MemoTrie
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Format

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
