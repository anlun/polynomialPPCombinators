{-# LANGUAGE DeriveGeneric #-}

module Doc where

import GHC.Generics (Generic)
import Data.Hashable

data Doc = Text String
         | Indent Int Doc
         | Doc `Beside` Doc
         | Doc `Above`  Doc
         | Doc `Choice` Doc
         deriving (Eq, Ord, Show, Generic)
instance Hashable Doc

text   = Text
indent = Indent

(>|<) :: Doc -> Doc -> Doc
a >|< b = Beside a b

(>-<) :: Doc -> Doc -> Doc
a >-< b = Above a b

(>//<) :: Doc -> Doc -> Doc
a >//< b = Choice a b

