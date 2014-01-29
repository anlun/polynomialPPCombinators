{-# LANGUAGE DeriveGeneric #-}

module Doc where

import GHC.Generics (Generic)
import Data.Hashable

data Doc = Text String
         | Indent Int Doc
         | Doc `Beside` Doc
         | Doc `Above`  Doc
         | Doc `Choice` Doc
         | OneLine Doc
         deriving (Eq, Ord, Show, Generic)
instance Hashable Doc

text    = Text
indent  = Indent
oneLine = OneLine

(>|<), (>-<), (>//<) :: Doc -> Doc -> Doc
a >|<  b = Beside a b
a >-<  b = Above  a b
a >//< b = Choice a b

--------------------------
--- Additional combinators

(>#<) :: Doc -> Doc -> Doc
a >#< b = a >|< text " " >|< b

addSeparator :: (Doc -> Doc) -> [Doc] -> [Doc]
addSeparator sep ls | length ls <= 1 = ls
addSeparator sep (l:ls) = (:) (sep l) $ addSeparator sep ls

hsep, h1sep, vsep :: [Doc] -> Doc
hsep  = foldl (>|<) (text "")
h1sep = (.) (foldl (>|<) (text "")) $ map oneLine
vsep  = foldl (>-<) (text "")
