{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeDirectedGeneric.SystemF.Pretty () where

import Common.Types
import Common.PrettyUtils
import Prettyprinter

import TypeDirectedGeneric.SystemF.Syntax

instance Pretty VarName where
  pretty (VarName x) = text x

instance Pretty TyVarName where
  pretty (TyVarName a) = text a

instance Pretty ConstrName where
  pretty (ConstrName t) = text t

funAppPrec :: Prec
funAppPrec = maxPrec

precUnOp :: UnOp -> Int
precUnOp op =
    case op of
      Not -> funAppPrec
      Inv -> funAppPrec

instance Pretty Exp where
    pretty = prettyPrec 0

instance PrettyPrec Exp where
  prettyPrec prec exp =
    case exp of
      ExpVar v -> pretty v
      ExpConstr k [] [] ->
        pretty k
      ExpConstr k [] args ->
        pretty k <> text "{" <> prettyCommas args <> text "}"
      ExpConstr k tys args ->
        pretty k <> text "@<" <> prettyCommas tys <> text "> {" <> prettyCommas args <> text "}"
      ExpApp e1 e2 ->
        withParens prec funAppPrec $
        prettyPrec funAppPrec e1 <+> prettyPrec funAppPrec e2
      ExpAbs x t e ->
        withParens prec 1 $
        text "λ" <> pretty x <> text ":" <> pretty t <+> dot <+> pretty e
      ExpTyApp e t ->
        withParens prec funAppPrec $
        prettyPrec funAppPrec e <> text "@" <> prettyPrec funAppPrec t
      ExpTyAbs a e ->
        withParens prec 1 $
        text "Λ" <> pretty a <+> dot <+> pretty e
      ExpCase e clauses ->
        withParens prec 1 $
        let e' = pretty e
            clauses' = map pretty clauses
        in align (text "case" <+> e' <+> text "of" <> line <>
                  indent 2 (vcat clauses') <> line)
      ExpBinOp e1 op e2 ->
        prettyPrec (precBinOp op) e1 <+> pretty op <+> prettyPrec (precBinOp op) e2
      ExpUnOp op e ->
        pretty op <+> prettyPrec (precUnOp op) e
      ExpCond e1 e2 e3 ->
        withParens prec 1 $
        text "if" <+> pretty e1 <+> text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      ExpInt i -> pretty i
      ExpBool b -> pretty b
      ExpStr t -> pretty (show t)
      ExpChar c -> pretty (show c)
      ExpFail s args ->
        withParens prec funAppPrec $
        text "error" <+> pretty (show s) <+>
        sepBy space (map (prettyPrec funAppPrec) args)
      ExpVoid -> text "void"
      ExpPrintf t args ->
        text "printf(" <+> pretty (show t) <+> text ", " <+> prettyCommas args <+> text ")"
      ExpSprintf t args ->
        text "sprintf(" <+> pretty (show t) <+> text ", " <+> prettyCommas args <+> text ")"

instance Pretty BinOp where
    pretty op =
      text $
      case op of
        Plus -> "+"
        Minus -> "-"
        Mult -> "*"
        Div -> "/"
        Mod -> "%"
        Equal -> "=="
        NotEqual -> "/="
        Lt -> "<"
        LtEqual -> "<="
        Gt -> ">"
        GtEqual -> ">="
        And -> "&&"
        Or -> "||"

instance Pretty UnOp where
    pretty Not = "!"
    pretty Inv = "-"

instance Pretty PatClause where
  pretty (PatClause pat exp) =
    pretty pat <+> text "->" <+> pretty exp

instance Pretty Pat where
  pretty = prettyPrec 0

instance PrettyPrec Pat where
  prettyPrec prec pat =
    case pat of
      PatVar v t ->
        withParens prec funAppPrec $
        pretty v <> text ":" <> pretty t
      PatWild t ->
        withParens prec funAppPrec $
        text "_:" <> pretty t
      PatConstr c tys pats ->
        withParens prec funAppPrec $
        let pats' = map (prettyPrec funAppPrec) pats
            tys' = map (prettyPrec funAppPrec) tys
        in pretty c <> text "@" <> (sepBy space tys') <> braces (sepBy comma pats')

instance Pretty PrimTy where
  pretty t =
    text $
    case t of
      PrimInt -> "Int"
      PrimBool -> "Bool"
      PrimString -> "String"
      PrimChar -> "Char"
      PrimVoid -> "Void"

instance Pretty Ty where
  pretty = prettyPrec 0

instance PrettyPrec Ty where
  prettyPrec prec ty =
    case ty of
      TyVar a -> pretty a
      TyPrim t -> pretty t
      TyArrow ty1 ty2 ->
        withParens prec funAppPrec $
        prettyPrec funAppPrec ty1 <+> text "->" <+> pretty ty2
      TyConstr c tys ->
        withParens prec funAppPrec $
        pretty c <> text "@" <>
        sepBy space (map (prettyPrec funAppPrec) tys)
      TyForall a ty ->
        withParens prec funAppPrec $
        text "∀" <> pretty a <> text "." <> pretty ty

instance Pretty Decl where
  pretty d =
    case d of
      DeclData c as tys ->
        text "data" <+> pretty c <+> text ":" <+>
        text "∀" <> (sepBy space (map pretty as)) <> text "." <>
        sepBy space (map pretty tys)
      DeclFun x t e ->
        align (text "fun" <+> pretty x <+> text ":" <+> pretty t <+> text "=" <> line <>
               (indent 2 (pretty e)))

instance Pretty Prog where
  pretty (Prog decls e) =
    sepBy line (map pretty decls) <> line <> pretty e
