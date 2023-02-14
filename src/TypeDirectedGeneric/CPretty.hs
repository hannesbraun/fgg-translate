{-# LANGUAGE OverloadedStrings #-}
module TypeDirectedGeneric.CPretty where

import Prettyprinter

import TypeDirectedGeneric.C
import Common.PrettyUtils
import Common.Types

instance Pretty TyName where
  pretty (TyName t) = text t

instance Pretty FunName where
  pretty (FunName t) = text t

instance Pretty Fld where
  pretty (Fld t) = text t

instance Pretty Var where
  pretty (Var t) = text t

instance Pretty Ty where
  pretty (Ty n k) =
    pretty n <>
    (case k of
       TkPtr -> text " *"
       TkVal -> mempty)

instance Pretty a => Pretty (PComp a) where
  pretty (PComp x mi) =
    case mi of
      Just (Idx i) -> pretty x <> brackets (pretty i)
      Nothing -> pretty x

instance Pretty Path where
  pretty (Path e flds) =
    sepBy (text ".") (pretty e : map pretty flds)

instance Pretty Stmts where
  pretty (Stmts l) =
    sepBy line (map pretty l)

instance Pretty Stmt where
  pretty stmt =
    case stmt of
      SReturn e ->
        text "return" <+> pretty e <> semi
      SDeclVar t v mn ->
        pretty t <+> pretty v <>
        (case mn of
           Just (Len i) -> brackets (pretty i)
           Nothing -> mempty) <>
        semi
      SAssign p e ->
        pretty p <+> text "=" <+> pretty e <> semi
      SIf e stmts1 stmts2 ->
        text "if (" <> pretty e <> ") {" <> line <>
        indent 2 (pretty stmts1) <>
        "}" <>
        (case stmts2 of
           (Stmts []) -> mempty
           (Stmts [s@(SIf _ _ _)]) -> text " else " <> pretty s
           _ -> text " else {" <> line <> indent 2 (pretty stmts2))
      SExp e -> pretty e <> semi

renderStdList :: Pretty a => [a] -> Doc ann
renderStdList [] = text "()"
renderStdList x = renderList "(" "," x ")"

instance Pretty Exp where
    pretty = prettyPrec 0

instance PrettyPrec Exp where
  prettyPrec prec exp =
    case exp of
      EVar v -> pretty v
      EPath p -> pretty p
      ECast ty e ->
        withParens prec precCast $
        parens (pretty ty) <> pretty e
      EDref e ->
        withParens prec precDref $
        text "*" <> prettyPrec maxPrec e
      ERef e ->
        withParens prec precRef $
        text "*" <> prettyPrec maxPrec e
      ECall f args ->
        pretty f <> renderStdList args
      EBinOp el op er ->
        prettyPrec (precBinOp op) el <+> pretty op <+> prettyPrec (precBinOp op) er
      EUnOp op e ->
        pretty op <+> prettyPrec (precUnOp op) e
      EArrCons es -> renderList "{" "," es "}"

precCast :: Prec
precCast = maxPrec

precDref :: Prec
precDref = maxPrec

precRef :: Prec
precRef = maxPrec

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
        NotEqual -> "!="
        Lt -> "<"
        LtEqual -> "<="
        Gt -> ">"
        GtEqual -> ">="
        And -> "&&"
        Or -> "||"

precUnOp :: UnOp -> Int
precUnOp op =
    case op of
      Not -> maxPrec
      Inv -> maxPrec

instance Pretty UnOp where
    pretty Not = "!"
    pretty Inv = "-"

instance Pretty Decl where
  pretty d =
    case d of
      DStruct s -> pretty s
      DUnion u -> pretty u
      DFun f -> pretty f

instance Pretty StructDecl where
  pretty (StructDecl name flds) =
    text "typedef struct {" <> line <>
    indent 2 (termBy semi (map (\(ty, f) -> pretty ty <+> pretty f) flds)) <> line <>
    text "} " <> pretty name <> semi

instance Pretty UnionDecl where
  pretty (UnionDecl name alts) =
    text "typedef struct {" <> line <>
    indent 2 (termBy semi (map (\(ty, f) -> pretty ty <+> pretty f) alts)) <> line <>
    text "} " <> pretty name <> semi

instance Pretty FunDecl where
  pretty (FunDecl name args res body) =
    pretty res <+> pretty name <>
    parens (sepBy comma (map (\(t, x) -> pretty t <+> pretty x) args)) <> line <>
    brackets (indent 2 (pretty body))

instance Pretty Prog where
  pretty (Prog ds) = vcat (map pretty ds)
