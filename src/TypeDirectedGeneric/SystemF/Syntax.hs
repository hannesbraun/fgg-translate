{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.SystemF.Syntax where

import Common.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Data hiding (Constr)
import qualified Data.Text as T
import Data.String
import Test.Framework

newtype VarName = VarName { unVarName :: T.Text }
    deriving (Eq, Ord, Data, IsString, Typeable)

instance Show VarName where
  showsPrec p (VarName t) =
    showParen (p > 10) $ showString "VarName " . showString (T.unpack t)

newtype TyVarName = TyVarName { unTyVarName :: T.Text }
    deriving (Eq, Ord, Data, IsString, Typeable)

instance Show TyVarName where
  showsPrec p (TyVarName t) =
    showParen (p > 10) $ showString "TyVarName " . showString (T.unpack t)

-- Constructors
newtype ConstrName = ConstrName { unConstrName :: T.Text }
    deriving (Eq, Ord, Data, IsString, Typeable)

instance Show ConstrName where
  showsPrec p (ConstrName t) =
    showParen (p > 10) $ showString "ConstrName " . showString (T.unpack t)

data Exp
    = ExpVar VarName
    | ExpConstr ConstrName [Ty] [Exp]
    | ExpApp Exp Exp
    | ExpAbs VarName Ty Exp
    | ExpTyApp Exp Ty
    | ExpTyAbs TyVarName Exp
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
    deriving (Eq, Show, Data, Typeable)

instance IsString Exp where
  fromString s = ExpVar (VarName (T.pack s))

data PatClause
    = PatClause Pat Exp
    deriving (Eq, Show, Data, Typeable)

data Pat
    = PatVar VarName Ty
    | PatWild Ty
    | PatConstr ConstrName [Ty] [Pat]
    deriving (Eq, Show, Data, Typeable)

data PrimTy
  = PrimInt
  | PrimBool
  | PrimString
  | PrimChar
  | PrimVoid
    deriving (Eq, Show, Data, Typeable)

data Ty
  = TyVar TyVarName
  | TyPrim PrimTy
  | TyArrow Ty Ty
  | TyConstr ConstrName [Ty]
  | TyForall TyVarName Ty
    deriving (Show, Data, Typeable)

type BiMap a b = (Map a b, Map b a)

tyEqAlpha :: Ty -> BiMap TyVarName TyVarName -> Ty -> Bool
tyEqAlpha ty1 vars@(forall1To2, forall2To1) ty2 =
  case (ty1, ty2) of
     (TyVar a, TyVar b) ->
       case (M.lookup a forall1To2, M.lookup b forall2To1) of
         (Nothing, Nothing) -> a == b -- both not forall quantified
         (Just b', Just a') -> a == a' && b == b' -- both forall quantified
         _ -> False
     (TyPrim p1, TyPrim p2) -> p1 == p2
     (TyArrow ty1 ty1', TyArrow ty2 ty2') ->
       tyEqAlpha ty1 vars ty2 &&
       tyEqAlpha ty1' vars ty2'
     (TyConstr c1 tys1, TyConstr c2 tys2) ->
       c1 == c2 && listEq tys1 tys2
     (TyForall a ty1, TyForall b ty2) ->
       tyEqAlpha ty1 (M.insert a b forall1To2, M.insert b a forall2To1) ty2
     _ -> False
  where
    listEq [] [] = True
    listEq (ty1:rest1) (ty2:rest2) =
      tyEqAlpha ty1 vars ty2 && listEq rest1 rest2
    listEq _ _ = False

instance Eq Ty where
  ty1 == ty2 = tyEqAlpha ty1 (M.empty, M.empty) ty2

test_eqTy :: IO ()
test_eqTy = do
  subAssert $ check False (TyVar "a") (TyVar "b")
  subAssert $ check True (TyVar "a") (TyVar "a")
  subAssert $ check True (TyForall "a" (TyVar "a")) (TyForall "b" (TyVar "b"))
  subAssert $ check True (TyForall "a" (TyVar "a")) (TyForall "a" (TyVar "a"))
  subAssert $ check False (TyForall "a" (TyVar "a")) (TyForall "b" (TyVar "a"))
  subAssert $ check False (TyForall "a" (TyVar "b")) (TyForall "b" (TyVar "b"))
  subAssert $ check True
    (TyForall "a" (TyForall "b" (TyVar "a"))) (TyForall "b" (TyForall "a" (TyVar "b")))
  subAssert $ check True
    (TyForall "a" (TyForall "b" (TyVar "b"))) (TyForall "b" (TyForall "a" (TyVar "a")))
  subAssert $ check False
    (TyForall "a" (TyForall "b" (TyVar "a"))) (TyForall "b" (TyForall "a" (TyVar "a")))
  subAssert $ check False
    (TyForall "a" (TyForall "b" (TyVar "b"))) (TyForall "b" (TyForall "a" (TyVar "b")))
  subAssert $ check False
    (TyForall "a" (TyForall "b" "a")) (TyForall "c" (TyForall "c" "c"))
  where
    check expected ty1 ty2 = do
      assertEqual expected (ty1 == ty2)
      assertEqual expected (ty2 == ty1)

instance IsString Ty where
  fromString s = TyVar (TyVarName (T.pack s))

data Decl
  = DeclData ConstrName [TyVarName] [Ty]
  | DeclFun VarName Ty Exp -- expression must be an abstraction
    deriving (Show, Data, Typeable)

data Prog = Prog [Decl] Exp
    deriving (Show, Data, Typeable)
