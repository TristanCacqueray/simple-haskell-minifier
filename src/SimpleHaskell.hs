-- | This module defines a simple Expr data type for minification purpose.
module SimpleHaskell where

import GHC.Data.FastString (FastString)

type Name = FastString

data Expr
    = -- | Literal: 42
      ELit Name
    | -- | Variable: x
      EVar Name
    | -- | Application: f x
      EApp Expr Expr
    | -- | Lambda: \x -> x
      ELam BindingMatch
    | -- | Case expression
      ECase Expr [BindingMatch]
    | -- | Do expression
      EDo [Statement]
    | -- | List comprehension
      EComp [Statement]
    | -- | Operator: e1 (op) e2
      EOp Expr Expr Expr
    | -- | List: [e1, e2, ...]
      EList [Expr]
    | -- | Tuple: (e1, e2, ...)
      ETuple [Expr]
    | -- | Parenthesis: (expr)
      EPar Expr
    | -- | Let binding: let v1 = e1; v2 = e2 in expr
      ELet [Binding] Expr
    | -- | If: if e1 then e2 else e3
      EIf Expr Expr Expr
    | -- | [e1..e2]
      ERange Expr (Maybe Expr) (Maybe Expr)
    deriving (Show)

data Statement
    = SBind Pattern Expr
    | SBody Expr
    deriving (Show)

-- | Expr with guard: |e1,e2,.. = e
data GuardedExpr = GuardedExpr [Expr] Expr
    deriving (Show)

data Pattern
    = -- | Binding pattern: x
      PVar Name
    | -- | Litteral pattern: 42
      PLit Name
    | -- | List of binding: (p1, p2, ...)
      PTup [Pattern]
    | -- | List pattern: [p1, p2]
      PList [Pattern]
    | -- | Named pat: x@p
      PNam Name Pattern
    | -- | Infix constructor pat: p1:p2
      PIco Name Pattern Pattern
    | -- | Prefix constructor pat: C p1 p2 ...
      PCon Name [Pattern]
    | -- | Parenthesis around pattern
      PPar Pattern
    deriving (Show)

-- | Binding declaration: pattern | guard = expr
data BindingMatch = BindingMatch [Pattern] [GuardedExpr]
    deriving (Show)

-- | Binding
data Binding
    = -- | f x = expr
      Binding Name [BindingMatch]
    | -- | (x,y) = expr
      BindingPattern Pattern [GuardedExpr]
    deriving (Show)

-- | Top level declaration
data Decl
    = ValueDecl Binding
    | SigDecl Name
    | TypeDecl Name
    deriving (Show)

data Import = Import Name [Name]
    deriving (Show)

data Module = Module [Import] [Decl]
    deriving (Show)
