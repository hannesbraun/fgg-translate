{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.UntypedTargetLanguage (

    Var(..), Constr(..), PatClause(..), Pat(..), Binding(..), Prog(..)
  , Exp(..)
  , expApp, expAppMany, expAbs, expAbsMany
  , evalProg, pairConstr, mkPair, tupleConstr, mkTuple, tuplePat, matchEq, matchEqFull
  , fstOfPair, sndOfPair, fstOfTriple, sndOfTriple, thdOfTriple, mkTriple, idFun, toString
  , printString, translateProg
  , htf_thisModulesTests

) where

import Common.Types
import Common.Utils
import Common.PrettyUtils
import Common.FGGPretty ()
import Prettyprinter
import TypeDirectedGeneric.SExp

import Data.Data hiding (Constr)
import Data.Generics hiding (Constr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.Framework
import Text.RawString.QQ
import System.Process
import Data.String
import System.Exit

newtype Var = Var { unVar :: T.Text }
    deriving (Eq, Ord, Show, Data, IsString, Typeable)

newtype Constr = Constr { unConstr :: T.Text }
    deriving (Eq, Ord, Show, Data, IsString, Typeable)

data Exp
    = ExpVar Var
    | ExpConstr Constr
    | ExpApp Exp [Exp]        -- nested applications
    | ExpAppMulti Exp [Exp]   -- function with multiple arguments
    | ExpAbs [Pat] Exp
    | ExpCase Exp [PatClause]
    | ExpTuple [Exp]
    | ExpList [Exp]
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

expApp :: Exp -> Exp -> Exp
expApp e1 e2
  | e1 == idFun = e2
  | otherwise = ExpApp e1 [e2]

expAppMany :: Exp -> Exp -> [Exp] -> Exp
expAppMany e1 e2 es = ExpApp e1 (e2:es)

expAbs :: Pat -> Exp -> Exp
expAbs p e = ExpAbs [p] e

expAbsMany :: Pat -> [Pat] -> Exp -> Exp
expAbsMany p ps e = ExpAbs (p:ps) e

data PatClause
    = PatClause Pat Exp
    deriving (Eq, Ord, Show, Data, Typeable)

data Pat
    = PatVar Var
    | PatWild
    | PatTuple [Pat]
    | PatList [Pat] -- for matching an exact number of elems
    | PatConstr Constr [Pat]
    deriving (Eq, Ord, Show, Data, Typeable)

data Binding = Binding Var [Pat] Exp
    deriving (Eq, Ord, Show, Data, Typeable)

data Prog
    = Prog [Binding] [Exp]
    deriving (Eq, Ord, Show, Data, Typeable)

expToSExp :: Exp -> SExp
expToSExp exp =
  case exp of
    ExpVar (Var v) -> SExpVar v
    ExpConstr (Constr k) -> SExpSym k
    ExpApp e es ->
      foldl (\se e -> SExp [app, se, expToSExp e]) (expToSExp e) es
    ExpAppMulti e es ->
      SExp (expToSExp e : map expToSExp es)
    ExpAbs pats body ->
      foldr (\p se -> SExp [matchLambda, SExp [SExp [patToSExp p], se]]) (expToSExp body) pats
    ExpCase e clauses -> SExp ([match, expToSExp e] ++ map clauseToSExp clauses)
    ExpTuple es ->
      case es of
        [] -> constrToSExp (tupleConstr 0)
        _ -> SExp ([list, constrToSExp (tupleConstr (length es))] ++ map expToSExp es)
    ExpList es -> SExp (list : map expToSExp es)
    ExpBinOp e1 op e2 -> SExp (transBinOp op : map expToSExp [e1, e2])
    ExpUnOp op e -> SExp [transUnOp op, expToSExp e]
    ExpCond e1 e2 e3 -> SExp (SExpVar "if" : map expToSExp [e1, e2, e3])
    ExpInt i -> SExpInt i
    ExpBool b -> SExpBool b
    ExpStr t -> SExpStr t
    ExpChar c -> SExpChar c
    ExpFail t args -> SExp ([error, SExpSym "ERROR", SExpStr t] ++ map expToSExp args)
    ExpVoid -> SExp [SExpVar "void"]
  where
    clauseToSExp (PatClause p e) = SExp [patToSExp p, expToSExp e]
    patToSExp p =
      case p of
        PatVar (Var v) -> SExpVar v
        PatWild -> SExpVar "_"
        PatList ps -> SExp (list : map patToSExp ps)
        PatTuple ps ->
          case ps of
            [] -> constrToSExp (tupleConstr 0)
            _ -> SExp ([list, constrToSExp (tupleConstr (length ps))] ++ map patToSExp ps)
        PatConstr k ps ->
          case ps of
            [] -> constrToSExp k
            _ -> SExp ([list, constrToSExp k] ++ map patToSExp ps)
    constrToSExp (Constr t) = SExpSym t
    app = SExpVar "app"
    list = SExpVar "list"
    match = SExpVar "match"
    matchLambda = SExpVar "match-lambda**"
    error = SExpVar "error"
    transBinOp op =
        SExpVar $
        case op of
          Plus -> "-add"
          Minus -> "-"
          Mult -> "*"
          Div -> "/"
          Mod -> "modulo"
          Equal -> "equal?"
          NotEqual -> "not-equal?"
          Lt -> "<"
          LtEqual -> "<="
          Gt -> ">"
          GtEqual -> ">="
          And -> "and"
          Or -> "or"
    transUnOp op =
        SExpVar $
        case op of
          Not -> "not"
          Inv -> "-"

bindingToSExp :: Binding -> SExp
bindingToSExp (Binding (Var v) pats body) =
  SExp [SExpVar "define", SExpVar v, expToSExp (ExpAbs pats body)]

progToSExps :: Prog -> SExps
progToSExps prog =
  let Prog bindings mainExps = everywhere (mkT renameVars) prog
  in SExps $ map bindingToSExp bindings ++ map expToSExp mainExps
  where
    renameVars :: Var -> Var
    renameVars v@(Var t) =
        if t `elem` forbiddenNames
        then Var ("__" <> t)
        else v
    forbiddenNames = ["app", "list", "match", "lambda", "modulo", "and", "or", "not", "define",
                      "require", "cond"]

racketHeader :: T.Text
racketHeader = "#lang racket\n" <> [r|
(require racket/match)

(define (app f x)
  (cond
   ((procedure? f) (f x))
   ((symbol? f) (list f x))
   ((list? f) (append f (list x)))))

(define (not-equal? x y) (not (equal? x y)))

(define (-fst x)
  (match x
   ((list 'tuple-2 a _) a)
   ((list 'tuple-3 a _ _) a)))

(define (-snd x)
  (match x
   ((list 'tuple-2 _ b) b)
   ((list 'tuple-3 _ b _) b)))

(define (-thd x)
  (match x
   ((list 'tuple-3 _ _ c) c)))

(define (-id x) x)

(define (-to-string fmt . args) (apply format (cons fmt (map -render args))))
(define (-print-string fmt . args)
  (display (apply format (cons fmt (map -render args)))))

; Called when converting to string. Extracts the struct part from an interface value
(define (-render x)
  (match x
   ((list 'tuple-3 _ s _) (-render s))
   (_ (if (list? x) (map -render x) x))))

(define (-add x y)
  (if (string? x) (string-append x y) (+ x y)))
|]

translateProg :: T.Text -> Prog -> T.Text
translateProg lib prog =
    let sexps = progToSExps prog
    in prettyToText (racketHeader <> "\n\n" <> lib <> "\n\n") sexps

evalProg :: FilePath -> T.Text -> Prog -> IO (Either T.Text T.Text)
evalProg outFile stdlib prog = do
  let code = translateProg stdlib prog
  T.writeFile outFile code
  (ecode, out, err) <- readProcessWithExitCode "racket" [outFile] ""
  case ecode of
    ExitSuccess -> pure $ Right (T.strip (T.pack out))
    ExitFailure _ -> pure $ Left (T.strip (T.pack err))

test_evalProg :: IO ()
test_evalProg = do
  subAssert $ checkEval "'(KPair 1 #t)" (mkProg (ExpInt 1) (ExpInt 2) (ExpInt 3))
  subAssert $ checkEval "1" (mkProg (ExpInt 0) (ExpInt 20) (ExpConstr kLeft))
  subAssert $ checkEval "2" (mkProg (ExpInt 0) (ExpInt 20) (ExpConstr kRight))
  where
    checkEval expected prog = do
      res <- evalProg "/tmp/fgg-target.rkt" "" prog
      case res of
        Right out -> assertEqual expected out
        Left err -> fail ("Evaluation failed: " ++ T.unpack err)
    mkProg :: Exp -> Exp -> Exp -> Prog
    mkProg e1 e2 e3 =
        -- let foo x (y, z) =
        --       if (\x -> x + 1) y < 10 then (x, true)
        --       else case z of
        --              Left -> 1
        --              Right -> 2
        -- in foo (e1, (e, e3))
        Prog
          [ Binding foo [PatConstr kPair [PatVar x, PatConstr kPair [PatVar y, PatVar z]]]
              (ExpCond
                 (ExpBinOp
                  (ExpApp (ExpAbs [PatVar x] (ExpBinOp (ExpVar x) Plus (ExpInt 1))) [ExpVar y])
                  Lt (ExpInt 10))
                 (ExpApp (ExpConstr kPair) [ExpVar x, ExpBool True])
                 (ExpCase (ExpVar z)
                              [ PatClause (PatConstr kLeft []) (ExpInt 1)
                              , PatClause (PatConstr kRight []) (ExpInt 2)]))
          ]
          [ExpApp (ExpVar foo) [ExpApp (ExpConstr kPair) [e1, ExpApp (ExpConstr kPair) [e2, e3]]]]
    foo = Var "foo"
    x = Var "x"
    y = Var "y"
    z = Var "z"
    kPair = Constr "KPair"
    kLeft = Constr "KLeft"
    kRight = Constr "KRight"

pairConstr :: Constr
pairConstr = tupleConstr 2

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = mkTuple [e1, e2]

mkTriple :: Exp -> Exp -> Exp -> Exp
mkTriple e1 e2 e3 = mkTuple [e1, e2, e3]

tupleConstr :: Int -> Constr
tupleConstr i = Constr (T.pack ("tuple-" ++ show i))

mkTuple :: [Exp] -> Exp
mkTuple = ExpTuple

tuplePat :: [Pat] -> Pat
tuplePat = PatTuple

-- matchEq E1 E2 E3: case E1 == E2 of True -> E3
matchEq :: Exp -> Exp -> Exp -> T.Text -> [Exp] -> Exp
matchEq e1 e2 e3 msg args =
  matchEqFull e1 e2 e3 (ExpFail msg args)

-- matchEqFull E1 E2 E3 E 4: case E1 == E2 of True -> E3; False -> E4
matchEqFull :: Exp -> Exp -> Exp -> Exp -> Exp
matchEqFull e1 e2 e3 e4 =
  ExpCond (ExpBinOp e1 Equal e2) e3 e4

fstOfPair :: Exp -> Exp
fstOfPair e = expApp (ExpVar "-fst") e

sndOfPair :: Exp -> Exp
sndOfPair e = expApp (ExpVar "-snd") e

fstOfTriple :: Exp -> Exp
fstOfTriple e = expApp (ExpVar "-fst") e

sndOfTriple :: Exp -> Exp
sndOfTriple e = expApp (ExpVar "-snd") e

thdOfTriple :: Exp -> Exp
thdOfTriple e = expApp (ExpVar "-thd") e

toString :: T.Text -> [Exp] -> Exp
toString fmt args =
  ExpAppMulti (ExpVar "-to-string") (ExpStr (rewriteFormat fmt) : args)

rewriteFormat :: T.Text -> T.Text
rewriteFormat = T.pack . rewriteFormat' . T.unpack
  where
    rewriteFormat' [] = []
    rewriteFormat' [c] = [c]
    rewriteFormat' ('%':'v':rest) = '~' : 'a' : rewriteFormat' rest
    rewriteFormat' ('%':'#':'v':rest) = '~' : 'a' : rewriteFormat' rest
    rewriteFormat' ('%':'#':c:rest) = '~' : c : rewriteFormat' rest
    rewriteFormat' ('%':c:rest) = '~' : c : rewriteFormat' rest
    rewriteFormat' (c:rest) = c : rewriteFormat' rest

printString :: T.Text -> [Exp] -> Exp
printString fmt args =
  ExpAppMulti (ExpVar "-print-string") (ExpStr (rewriteFormat fmt) : args)

idFun :: Exp
idFun = ExpVar "-id"

--
-- Pretty printing
--

funAppPrec :: Prec
funAppPrec = maxPrec

precUnOp :: UnOp -> Int
precUnOp op =
    case op of
      Not -> funAppPrec
      Inv -> funAppPrec

instance Pretty Var where
  pretty (Var v) = text v

instance Pretty Constr where
  pretty (Constr k) = text k

instance Pretty Exp where
    pretty = prettyPrec 0

instance PrettyPrec Exp where
  prettyPrec prec exp =
    case exp of
      ExpVar v -> pretty v
      ExpConstr k -> pretty k
      ExpApp e [] ->
        withParens prec funAppPrec $
        prettyPrec funAppPrec e
      ExpApp e1 (e2:es) ->
        foldl
            (\f arg ->
                withParens prec funAppPrec $ f <+> prettyPrec funAppPrec arg)
            (withParens prec funAppPrec $
             prettyPrec funAppPrec e1 <+>
             prettyPrec funAppPrec e2)
            es
      ExpAppMulti e es ->
        withParens prec funAppPrec $
        let e' = prettyPrec funAppPrec e
            es' = map (prettyPrec funAppPrec) es
        in hang 2 $ sep (e' : es')
      ExpAbs [] _ -> error ("found abstraction with no parameters: " ++ show exp)
      ExpAbs pats e ->
        withParens prec 1 $
        let pats' = map (prettyPrec funAppPrec) pats
            e' = pretty e
        in text "λ" <> sepBy space pats' <+> dot <+> e'
      ExpCase e clauses ->
        withParens prec 1 $
        let e' = pretty e
            clauses' = map pretty clauses
        in align (text "case" <+> e' <+> text "of [" <> line <>
                  indent 2 (vcat clauses') <> line <> indent (-2) (text "]"))
      ExpBinOp e1 op e2 ->
        prettyPrec (precBinOp op) e1 <+> pretty op <+> prettyPrec (precBinOp op) e2
      ExpUnOp op e ->
        pretty op <+> prettyPrec (precUnOp op) e
      ExpCond e1 e2 e3 ->
        withParens prec 1 $
        text "if" <+> pretty e1 <+> text "then" <+> pretty e2 <+>
        text "else" <+> pretty e3
      ExpTuple es -> parens (sepBy (text ", ") (map pretty es))
      ExpList es -> brackets (sepBy (text ", ") (map pretty es))
      ExpInt i -> pretty i
      ExpBool b -> pretty b
      ExpStr t -> pretty (show t)
      ExpChar c -> pretty (show c)
      ExpFail s args ->
        withParens prec funAppPrec $
        text "error" <+> pretty (show s) <+>
        sepBy space (map (prettyPrec funAppPrec) args)
      ExpVoid -> text "void"

instance Pretty Pat where
  pretty = prettyPrec 0

instance PrettyPrec Pat where
  prettyPrec prec pat =
    case pat of
      PatVar v -> pretty v
      PatWild -> pretty (Var "_")
      PatTuple ps -> parens (sepBy (text ", ") (map pretty ps))
      PatList ps -> brackets (sepBy (text ", ") (map pretty ps))
      PatConstr c pats ->
        withParens prec funAppPrec $
        let pats' = map (prettyPrec funAppPrec) pats
        in pretty c <+> sepBy space pats'

instance Pretty PatClause where
  pretty (PatClause pat exp) =
    pretty pat <+> text "->" <+> pretty exp

instance Pretty UnOp where
    pretty Not = "not"
    pretty Inv = "-"
