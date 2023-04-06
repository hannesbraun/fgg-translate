{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeDirectedGeneric.SystemF (

  typeCheck, erase

  ) where

import Common.Types
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL

import Data.Data hiding (Constr)
import qualified Data.Text as T
import Data.String

newtype Var = Var { unVar :: T.Text }
    deriving (Eq, Ord, Show, Data, IsString, Typeable)

newtype TyVar = TyVar { unTyVar :: T.Text }
    deriving (Eq, Ord, Show, Data, IsString, Typeable)

-- Constructors
newtype Constr = Constr { unConstr :: T.Text }
    deriving (Eq, Ord, Show, Data, IsString, Typeable)

data Exp
    = ExpVar Var
    | ExpConstr Constr [Type] [Exp]
    | ExpApp Exp Exp
    | ExpAbs Var Type Exp
    | ExpTyApp Exp Type
    | ExpTyAbs TyVar Exp
    | ExpCase Exp [PatClause]
    | ExpBinOp Exp BinOp Exp
    | ExpUnOp UnOp Exp
    | ExpCond Exp Exp Exp
    | ExpInt Integer
    | ExpBool Bool
    | ExpStr T.Text
    | ExpChar Char
    | ExpFail T.Text [Exp]
    | ExpVoid
    deriving (Eq, Ord, Show, Data, Typeable)

data PatClause
    = PatClause Pat Exp
    deriving (Eq, Ord, Show, Data, Typeable)

data Pat
    = PatVar Var Type
    | PatWild
    | PatConstr Constr [Pat]
    deriving (Eq, Ord, Show, Data, Typeable)

data Type
  = TypeVar TyVar
  | TypeArrow Type Type
  | TypeConstr Constr [Type]
  | TypeForall TyVar Type
    deriving (Eq, Ord, Show, Data, Typeable)

data Decl
  = DeclData Constr [TyVar] [Type]
  | DeclFun Var Type Exp -- expression must be an abstraction
    deriving (Eq, Ord, Show, Data, Typeable)

data Prog = Prog [Decl] Exp
    deriving (Eq, Ord, Show, Data, Typeable)

typeCheck :: Prog -> IO () -- crashes on type errors
typeCheck _ = pure () -- FIXME

erase :: Prog -> TL.Prog
erase = undefined -- FIXME
