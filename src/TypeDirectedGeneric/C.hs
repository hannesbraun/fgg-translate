module TypeDirectedGeneric.C where

import qualified Data.Text as T

import Common.Types

newtype TyName = TyName T.Text
newtype FunName = FunName T.Text
newtype Fld = Fld T.Text
newtype Var = Var T.Text

newtype Idx = Idx Int
newtype Len = Len Int

data TyKind = TkPtr | TkVal

data Ty = Ty TyName TyKind

data PComp a
  = PComp a (Maybe Idx)

data Path
  = Path (PComp Exp) [PComp Fld]

newtype Stmts = Stmts [Stmt]

data Stmt
  = SReturn Exp
  | SDeclVar Ty Var (Maybe Len) -- `T x` or `T x[3]`;
  | SAssign Path Exp
  | SIf Exp Stmts Stmts
  | SExp Exp

data Exp
  = EVar Var
  | EPath Path
  | ECast Ty Exp
  | EDref Exp -- \*e
  | ERef Exp -- &e
  | ECall FunName [Exp]
  | EBinOp Exp BinOp Exp
  | EUnOp UnOp Exp
  | EArrCons [Exp] -- {e1,...,en}

data StructDecl = StructDecl
  { sd_name :: TyName
  , sd_fields :: [(Ty, Fld)]
  }

data UnionDecl = UnionDecl
  { ud_name :: TyName
  , ud_alts :: [(Ty, Fld)]
  }

data FunDecl = FunDecl
  { fd_name :: FunName
  , fd_args :: [(Ty, Var)]
  , fd_return :: Ty
  , fd_body :: Stmts
  }

data Decl
  = DStruct StructDecl
  | DUnion UnionDecl
  | DFun FunDecl

newtype Prog = Prog [Decl]
