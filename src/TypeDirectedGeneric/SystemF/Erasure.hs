module TypeDirectedGeneric.SystemF.Erasure (
  erase
  ) where

import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL
import TypeDirectedGeneric.SystemF.Syntax

import Data.Maybe

transVarName :: VarName -> TL.Var
transVarName (VarName x) = TL.Var x

transConstrName :: ConstrName -> TL.Constr
transConstrName (ConstrName x) = TL.Constr x

eraseExp :: Exp -> TL.Exp
eraseExp exp =
  case exp of
    ExpVar x ->
      TL.ExpVar (transVarName x)
    ExpConstr c _ args ->
      TL.ExpApp (TL.ExpConstr (transConstrName c)) (map eraseExp args)
    ExpApp e1 e2 ->
      TL.ExpApp (eraseExp e1) [eraseExp e2]
    ExpAbs x _ e ->
      TL.ExpAbs [TL.PatVar (transVarName x)] (eraseExp e)
    ExpTyApp e _ ->
      eraseExp e
    ExpTyAbs _ e ->
      eraseExp e
    ExpCase e clauses ->
      TL.ExpCase (eraseExp e) (map eraseClause clauses)
    ExpBinOp e1 op e2 ->
      TL.ExpBinOp (eraseExp e1) op (eraseExp e2)
    ExpUnOp op e ->
      TL.ExpUnOp op (eraseExp e)
    ExpCond e1 e2 e3 ->
      TL.ExpCond (eraseExp e1) (eraseExp e2) (eraseExp e3)
    ExpInt i ->
      TL.ExpInt i
    ExpBool b ->
      TL.ExpBool b
    ExpStr t ->
      TL.ExpStr t
    ExpChar c ->
      TL.ExpChar c
    ExpFail msg args ->
      TL.ExpFail msg (map eraseExp args)
    ExpVoid ->
      TL.ExpVoid
    ExpPrintf t args ->
      TL.printString t (map eraseExp args)
    ExpSprintf t args ->
      TL.toString t (map eraseExp args)

eraseClause :: PatClause -> TL.PatClause
eraseClause (PatClause pat exp) = TL.PatClause (erasePat pat) (eraseExp exp)

erasePat :: Pat -> TL.Pat
erasePat p =
  case p of
    PatVar x _ -> TL.PatVar (transVarName x)
    PatWild _ -> TL.PatWild
    PatConstr c _ pats -> TL.PatConstr (transConstrName c) (map erasePat pats)

eraseDecl :: Decl -> Maybe TL.Binding
eraseDecl decl =
  case decl of
    DeclData {} -> Nothing
    DeclFun f _ e -> Just $ TL.Binding (transVarName f) [] (eraseExp e)

erase :: Prog -> TL.Prog
erase p =
  case p of
    Prog decls exp ->
      let bindings = mapMaybe eraseDecl decls
          tlExp = eraseExp exp
      in TL.Prog bindings [tlExp]
