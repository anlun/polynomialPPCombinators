module AltPretty where

--import Data.MemoTrie
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Format
import Variants

type Doc = Int -> Variants

text :: String -> Doc
text s n = checkUpdate n (s2fmt s) Map.empty 

indent :: Int -> Doc -> Doc
indent i d n = Map.fromList vs where
 vars = Map.elems $ Map.filter (isSuitable (n-i)) (d n)
 vs   = map ((\f -> (fmtToFrame f, f)) . indentFmt i) vars

choice, beside, above :: Doc -> Doc -> Doc
choice a b n = Map.foldl' (flip update) (b n) (a n)
beside a b   = cross besideFmt a b
above  a b   = cross aboveFmt  a b

cross :: (Format -> Format -> Format) -> Doc -> Doc -> Doc
cross f a b n = Map.foldl' bFold Map.empty (a n) where
 bFold m fa = Map.foldl' (flip $ checkUpdate n . f fa) m bv
 bv         = b n
  
(>//<), (>|<), (>-<) :: Doc -> Doc -> Doc
((>//<), (>|<), (>-<)) = (choice, beside, above)

pretty :: Int -> Doc -> String
pretty n d = case Map.elems (d n) of
              [] -> error "No layout"
              xs -> (\x -> txtstr x 0 "") $ List.minimum xs
