module Core where

import Pretty
import Doc

type Name       = String
type Parameters = [Name]

data Stmt = Print Expr
          | Inc Name
          | Assign Name Expr
          | Seq [Stmt]
          | FunDecl Name Parameters Stmt

data Expr = Const Int
          | Var Name
          | Binop String Expr Expr
          | FunCall Name [Expr]

type Program = [Stmt]

------------------
-- Pretty-Printer

expPP :: Expr -> Doc
expPP (Const c) = text $ show c
expPP (Var   x) = text x
expPP (Binop op l r) = (lod >#< rd) >//< (lod >-< rd)
  where
    od = text op
    ld = expPP l
    lod = ld >#< od
    rd = expPP r
expPP (FunCall name parameters) = (nd >|< hp) >//< ((nd >|< vp) >-< text ")")
  where
    nd  = (text name) >|< text "("
    pds = addSeparator (>|< text ",") $ map expPP parameters
    hp  = (h1sep pds) >|< text ")"
    vp  = vsep pds

opPP :: Stmt -> Doc
opPP (Print    e) = text "print" >#< expPP e 
opPP (Inc      n) = text n >|< text "++"
opPP (Assign n e) = text n >#< text "=" >#< expPP e
opPP (Seq     []) = text ""
opPP (Seq (x:xs)) =
