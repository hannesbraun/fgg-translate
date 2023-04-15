{-# LANGUAGE OverloadedStrings #-}
module Common.HaskellPretty where

import Common.HaskellAST
import Common.Types
import Common.PrettyUtils

import Prettyprinter
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T

instance Pretty Prog where
    pretty prog =
        let lst = map pretty (p_decls prog)
        in vcatBy (line <> line) lst <> line <> line <> pretty (p_main prog)

instance Pretty Main where
    pretty main =
        let bindings =
                vcatBy line $ flip map (m_bindings main)  $ \(x, e) ->
                    text "!" <> pretty x <+> text "= force $" <+> pretty e
        in text "main = " <> align (text "let " <> (align bindings) <> line <>
                                    text "in print" <+> (parens (pretty (m_result main))))

instance Pretty Decl where
    pretty decl =
        case decl of
          DeclClass x -> pretty x
          DeclInst x -> pretty x
          DeclData x -> pretty x
          DeclVal x -> pretty x
          DeclNewtype x -> pretty x

renderContext :: [Constr] -> Doc ann
renderContext l =
    case l of
      [] -> emptyDoc
      [c] -> pretty c <+> text "=> "
      _ -> renderList "(" ", " l ") => "

instance Pretty ClassDecl where
    pretty (ClassDecl tyVars ctx clsName funDeps methods) =
        text "class" <+> renderContext ctx <>
        pretty clsName <+> hsep (map pretty tyVars) <+>
        renderList "| " "," funDeps " " <> text "where" <> line <>
        indent 2 (vcat (map pretty methods))

instance Pretty Constr where
    pretty c =
        let tyVars = Set.toList (c_tyVars c)
            tyVarsPretty = text "forall" <+> (vsep (map pretty tyVars)) <+> text "."
            lhs = c_lhs c
            lhsPretty = sepBy comma (map pretty lhs) <+> text "=>"
            rhsPretty = pretty (c_rhs c)
        in case (Set.toList (c_tyVars c),  c_lhs c) of
             ([], []) -> rhsPretty
             (_:_, []) -> parens $ tyVarsPretty <+> rhsPretty
             ([], _:_) -> parens $ lhsPretty <+> rhsPretty
             (_:_, _:_) -> parens $ tyVarsPretty <+> lhsPretty <+> rhsPretty

instance Pretty ClassConstr where
    pretty cc = pretty (cc_className cc) <+> hsep (map pretty (cc_types cc))

instance Pretty FunDep where
    pretty fd = hsep (map pretty $ fd_lhs fd) <+> text "->" <+> pretty (fd_rhs fd)

instance Pretty InstDecl where
    pretty (InstDecl flags _tyVars ctx clsName types methods) =
        text "instance" <+> pretty flags <> renderContext ctx <>
        pretty clsName <+> hsep (map pretty types) <+> text "where" <> line <>
        indent 2 (vcat (map pretty methods))

instance Pretty InstFlags where
    pretty (InstFlags set) =
        if InstOverlappable `Set.member` set
        then text "{-# OVERLAPPABLE #-} "
        else emptyDoc

prettyConstructor :: Tau -> Constructor -> Doc a
prettyConstructor defTy (Constructor name tyVars ctx args mResult) =
    let result = fromMaybe defTy mResult
    in pretty name <+> text "::" <+>
       renderList "forall " " " tyVars " . " <>
       renderContext ctx <>
       pretty (funType args result)

instance Pretty DataDecl where
    pretty (DataDecl name tyVars ctors deriv) =
        text "data" <+> pretty name <+> hsep (map pretty tyVars) <+> text "where" <> line <>
        indent 2 (prettyCtors <> prettyDeriv)
      where
        prettyCtors =
            vcat $
            map (prettyConstructor (TypeTyCon name (map tv tyVars))) ctors
        prettyDeriv =
            case deriv of
              [] -> emptyDoc
              classes ->
                  line <> text "deriving" <+> renderList "(" ", " classes ")"

instance Pretty NewtypeDecl where
    pretty (NewtypeDecl name constr un ty) =
        text "newtype" <+> pretty name <+> text "=" <+> pretty constr <+>
        text "{" <+> pretty un <+> text "::" <+> pretty ty <+> text "}"

instance Pretty ValDecl where
    pretty (ValDecl mSig name args exp) =
        (case mSig of
           Nothing -> emptyDoc
           Just t -> pretty name <+> text "::" <+> text t <> line) <>
        pretty name <+>
        hsep (map pretty args) <+>
        text "=" <+>
        pretty exp

instance Pretty MeDecl where
    pretty (MeDecl name sig) = pretty name <+> text "::" <+> pretty sig

instance Pretty MethodDef where
    pretty (MethodDef name exp) = pretty name <+> text "=" <+> pretty exp

instance Pretty Exp where
    pretty = prettyPrec 0

instance PrettyPrec Exp where
    prettyPrec prec exp =
        case exp of
          ExpVar v -> pretty v
          ExpDataCon k ->
              case isTupleDataCon k of
                Just 0 -> text "()"
                Just 1 -> text "id"
                Just n ->
                    let vars = map (\i -> "x" ++ show i) (take n [0..])
                    in parens (text "\\" <> hsep (map pretty vars) <+> text "->" <+>
                               renderList "(" "," vars ")")
                Nothing -> pretty k
          ExpApp e1 e2 ->
              withParens prec funAppPrec $
              prettyPrec (funAppPrec - 1) e1 <+> prettyPrec funAppPrec e2
          ExpLet [] e -> prettyPrec prec e
          ExpLet bindings e ->
              let prettyBindings =
                      vcat $
                      flip map bindings $ \(x, e) ->
                      pretty x <+> text "=" <+> pretty e
              in withParens prec 1 $
                 align $
                 text "let" <+> (align prettyBindings) <> line <>
                 text "in" <+> pretty e
          ExpLambda x e ->
              withParens prec 1 $
              text "\\" <> pretty x <> text " -> " <> pretty e
          ExpCase e pats ->
              text "case" <+> pretty e <+> text "of" <> line <>
              indent 2 (vcat $ map (\(p, e) -> pretty p <+> text "->" <+> pretty e) pats)
          ExpIntLit i ->
              pretty i
          ExpStrLit t ->
              pretty (show t)
          ExpCharLit c ->
              pretty (show c)
          ExpBinOp op left right ->
              prettyPrec (precBinOp op) left <+> pretty op <+> prettyPrec (precBinOp op) right
          ExpUnOp op e ->
              pretty op <+> prettyPrec (precUnOp op) e
          ExpIte e1 e2 e3 ->
              withParens prec 1 $
                  text "if" <+> pretty e1 <+> text "then" <+> pretty e2 <+>
                  text "else" <+> pretty e3

instance Pretty BinOp where
    pretty op =
      text $
      case op of
        Plus -> "+"
        Minus -> "-"
        Mult -> "*"
        Div -> "`div`"
        Mod -> "`mod`"
        Equal -> "=="
        NotEqual -> "/="
        Lt -> "<"
        LtEqual -> "<="
        Gt -> ">"
        GtEqual -> ">="
        And -> "&&"
        Or -> "||"

funAppPrec :: Prec
funAppPrec = maxPrec

precUnOp :: UnOp -> Int
precUnOp op =
    case op of
      Not -> funAppPrec
      Inv -> funAppPrec

instance Pretty UnOp where
    pretty Not = "not"
    pretty Inv = "-"

instance Pretty Pat where
    pretty (Pat name vars) =
        case isTupleDataCon name of
          Just 0 -> text "()"
          Just _ -> renderList "(" ", " vars ")"
          Nothing -> pretty name <+> (hsep $ map pretty vars)

instance Pretty Tau where
    pretty = prettyPrec 0

instance PrettyPrec Tau where
    prettyPrec prec tau =
        case tau of
          TypeTyVar x -> pretty x
          TypeArrow t1 t2 ->
              withParens prec funAppPrec $
              prettyPrec funAppPrec t1 <+> text "->" <+> pretty t2
          TypeTyCon t [] ->
              case isTupleTyCon t of
                Just 0 -> text "()"
                _ -> pretty t
          TypeTyCon t args ->
              case isTupleTyCon t of
                Just n | n == length args ->
                           case args of
                             [t] -> pretty t
                             _ -> renderList "(" ", " args ")"
                _ ->
                    withParens prec funAppPrec $
                    pretty t <+> (hsep $ map (prettyPrec funAppPrec) args)

instance Pretty Sigma where
    pretty (Sigma tyVars ctx tau) =
        renderList "forall " "" (Set.toList tyVars) " . " <>
        renderContext ctx <>
        pretty tau

instance Pretty ClassName where
    pretty (ClassName t) = text t

instance Pretty TyVarName where
    pretty (TyVarName t) = text t

instance Pretty TyConName where
    pretty (TyConName t) = text t

instance Pretty DataConName where
    pretty (DataConName t) = text t

instance Pretty VarName where
    pretty (VarName t) =
        let fixed =
                case T.uncons t of
                  Just (first, rest) -> T.cons (toLower first) rest
                  Nothing -> t
        in text fixed
