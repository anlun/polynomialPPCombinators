{-# LANGUAGE DeriveGeneric #-}

module Format where

import GHC.Generics (Generic)
import Data.Hashable

-- -------------------------------------------------------------------
-- From Swierstra UU Lib ---------------------------------------------

spaces = ' ':spaces
sp n = if n >= 0 then take n spaces else ""

type T_PW  = Int
type T_PLL = Int
type T_PH  = Int

data Format = Elem { height  :: T_PH
                   , last_w  :: T_PLL
                   , total_w :: T_PW
                   , txtstr  :: Int -> String -> String
                   }

instance Eq Format  where
  x == y =  height  x == height  y
         && total_w x == total_w y
         && last_w  x == last_w  y

instance Ord Format where
  x <  y =  height x <  height y
         || (height x == height y && total_w x < total_w y)
  min x y = if x < y then x else y

s2fmt     :: String -> Format
s2fmt s   = Elem 1 l l (\_ -> (s++))
  where l = length s

indentFmt :: Int -> Format -> Format
indentFmt i   (Elem dh dl dw dt)
   = Elem dh (i + dl) (i + dw) (\n -> ((sp i) ++) . dt (i + n))

aboveFmt, besideFmt :: Format -> Format -> Format
(Elem uh ul uw ut) `aboveFmt` (Elem lh ll lw lt)
  = Elem (uh + lh) ll (uw `max` lw)
         (makeTsAbove ut lt)
  where makeTsAbove ut lt = \n -> let nl_skip = (('\n':sp n)++)
                                  in  ut n . nl_skip . lt n
(Elem lh ll lw lt) `besideFmt` (Elem rh rl rw rt)
  = Elem (lh + rh - 1) (ll + rl)
         (lw `max` (ll + rw)) (\n -> lt n . rt (ll + n))

isSuitable :: Int -> Format -> Bool
isSuitable width fmt = (total_w fmt) <= width

data Frame = F Int Int
             deriving (Eq, Generic)

instance Hashable Frame
instance Ord Frame where
  max x@(F w lw) y@(F w' lw')
    | w >  w'             = x
    | w == w' && lw > lw' = x
    | otherwise           = y

fmtToFrame :: Format -> Frame
fmtToFrame fmt = F (total_w fmt) (last_w fmt)

isFrameSuitable :: Int -> Frame -> Bool
isFrameSuitable n (F w lw) = w <= n
