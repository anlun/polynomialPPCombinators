module Variants where

import qualified Data.HashMap.Strict as Map
import Format

type Variants = Map.HashMap Frame Format

update :: Format -> Variants -> Variants
update fmt = Map.insertWith min (fmtToFrame fmt) fmt 

checkUpdate :: Int -> Format -> Variants -> Variants
checkUpdate n f = if isSuitable n f then update f else id
