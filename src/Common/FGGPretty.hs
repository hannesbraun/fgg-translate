{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.FGGPretty where

import Common.FGGAST
import Common.Types
import Common.PrettyUtils
import Common.Utils
import Prettyprinter
import qualified Data.Text as T

instance Pretty MeName where
  pretty (MeName t) = text t

instance Pretty TyName where
  pretty (TyName t) = text t

instance Pretty TyVarName where
  pretty (TyVarName t) = text t

instance Pretty VarName where
  pretty (VarName t) = text t

instance Pretty FieldName where
  pretty (FieldName t) = text t

renderStdList :: Pretty a => [a] -> Doc ann
renderStdList x = renderList "(" "," x ")"

renderStdList' :: Pretty a => [a] -> Doc ann
renderStdList' [] = text "()"
renderStdList' x = renderList "(" "," x ")"

instance Pretty Type where
  pretty tau =
    case tau of
      TyVar v -> pretty v
      TyNamed t [] -> pretty t
      TyNamed t args -> pretty t <> renderStdList args

instance {-# OVERLAPPING #-} Pretty [Type] where
  pretty l = renderStdList' l

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

instance Pretty Exp where
    pretty = prettyPrec 0

instance PrettyPrec Exp where
  prettyPrec prec e =
    case e of
      Var v -> pretty v
      MeCall e m ts es ->
        pretty e <> dot <> pretty m <> renderStdList ts <> renderStdList' es
      FunCall m ts es ->
        pretty m <> renderStdList ts <> renderStdList' es
      StructLit t es ->
        pretty t <> text "{" <> renderList "" "," es "" <> text "}"
      Select e f ->
        pretty e <> dot <> pretty f
      TyAssert e t ->
        pretty e <> dot <> parens (pretty t)
      BinOp op e1 e2 ->
        prettyPrec (precBinOp op) e1 <+> pretty op <+> prettyPrec (precBinOp op) e2
      UnOp op e ->
        (case op of
          Not -> text "!"
          Inv -> text "-") <> prettyPrec maxPrec e
      Cond e1 e2 e3 ->
        withParens prec maxPrec $
        pretty e1 <+> text "?" <+> pretty e2 <+> text ":" <+> pretty e3
      BoolLit True -> text "true"
      BoolLit False -> text "false"
      IntLit i -> text (showText i)
      StrLit t -> text (T.pack (show t))
      CharLit c -> text (T.pack (show c))

instance Pretty TyFormals where
  pretty (TyFormals []) = mempty
  pretty (TyFormals l) =
    text "(type " <>
    sepBy comma (map prettyFormal l) <>
    text ")"
    where
      prettyFormal (tyVar, Nothing) = pretty tyVar
      prettyFormal (tyVar, Just t) = pretty tyVar <+> pretty t

instance Pretty MeSig where
  pretty sig =
    pretty (msig_tyArgs sig) <>
    prettyArgs (msig_args sig) <>
    pretty (msig_res sig)
    where
      prettyArgs [] = mempty
      prettyArgs l =
        text "(" <>
        sepBy comma (map (\(v, t) -> pretty v <+> pretty t) l) <>
        text ")"

instance Pretty MeSpec where
  pretty spec = pretty (ms_name spec) <> pretty (ms_sig spec)
