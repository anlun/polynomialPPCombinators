module Core where

import Pretty
import Doc

type Name = String
type Parameters = [Name]

data Op = Print Exp
        | Assign Name Exp
        | Seq Op Op
        | FunDecl Name Parameters Op

data Exp = Const Int
         | Var Name
         | Binop String Exp Exp
         | FunCall Name [Exp]

type Program = [Op]

------------------
-- Pretty-Printer

expPP :: Exp -> Doc
expPP (Const c) = text $ show c
expPP (Var   x) = text x
expPP (Binop op l r) = (lod >||< rd) >//< (lod >-< rd)
  where
    od = text op
    ld = expPP l
    lod = ld >||< od
    rd = expPP r
expPP (FunCall name parameters) = nd
  where
    nd  = text name
    pds = addSeparator (>|< text ",") $ map expPP parameters
