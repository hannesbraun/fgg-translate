{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.SystemF.Parser (
  FParser, parseTy, parseExp, parseProg, parsePatClause, runP, htf_thisModulesTests
) where

import TypeDirectedGeneric.SystemF.Syntax
import TypeDirectedGeneric.SystemF.Pretty ()
import Common.Types
import Common.PrettyUtils

import Text.Parsec ( (<|>), (<?>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P

import Test.Framework

import Data.Char
import Control.Monad.Reader
import qualified Data.Text as T

lang :: P.GenLanguageDef T.Text () (Reader ParserConfig)
lang =
    P.LanguageDef
    { P.commentStart = "{-"
    , P.commentEnd = "-}"
    , P.commentLine = "--"
    , P.nestedComments = True
    , P.reservedNames =
        ["data", "fun", "let", "case", "of", "if", "then", "else", "var"
        , "forall", "True", "False"]
    , P.reservedOpNames = ["->"] -- ["\\", "\\\\"]
    , P.identStart  = P.letter <|> P.char '_'
    , P.identLetter = P.alphaNum <|> P.oneOf "_'"
    , P.opStart = P.opLetter lang
    , P.opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.caseSensitive = True
    }

annot :: T.Text -> FParser a -> FParser a
annot s p = p <?> (T.unpack s)

lexer :: P.GenTokenParser T.Text () (Reader ParserConfig)
lexer = P.makeTokenParser lang

type ParserConfig = ()

type FParser = P.ParsecT T.Text () (Reader ParserConfig)

reserved :: T.Text -> FParser ()
reserved t = P.reserved lexer (T.unpack t)

reservedOp :: T.Text -> FParser ()
reservedOp t = P.reservedOp lexer (T.unpack t)

identifier :: FParser T.Text
identifier = P.identifier lexer >>= (pure . T.pack)

whiteSpace :: FParser ()
whiteSpace = P.whiteSpace lexer

parens :: FParser a -> FParser a
parens = P.parens lexer

-- between { and }
braces :: FParser a -> FParser a
braces = P.braces lexer

comma :: FParser ()
comma = P.comma lexer >> pure ()

stringLiteral :: FParser T.Text
stringLiteral = P.stringLiteral lexer >>= (pure . T.pack)

charLiteral :: FParser Char
charLiteral = P.charLiteral lexer

startsUpppercase :: T.Text -> Bool
startsUpppercase t =
  case T.uncons t of
    Just (c, _) -> isUpper c
    _ -> False

parseNameInExp :: FParser (Either ConstrName VarName)
parseNameInExp = annot "name" $ do
  x <- identifier
  pure $ if startsUpppercase x then Left (ConstrName x) else Right (VarName x)

parseNameInTy :: FParser (Either ConstrName TyVarName)
parseNameInTy = annot "name" $ do
  x <- identifier
  pure $ if startsUpppercase x then Left (ConstrName x) else Right (TyVarName x)

parseTyVarName :: FParser TyVarName
parseTyVarName = annot "type variable" $ do
  name <- parseNameInTy
  case name of
    Right a -> pure a
    Left _ -> fail "expected type variable"

parseVarName :: FParser VarName
parseVarName = annot "variable" $ do
  name <- parseNameInExp
  case name of
    Right a -> pure a
    Left _ -> fail "expected variable"

parseConstrName :: FParser ConstrName
parseConstrName = annot "constructor" $ do
  name <- parseNameInExp
  case name of
    Right _ -> fail "expected constructor"
    Left c -> pure c

parseDecl :: FParser Decl
parseDecl = annot "declaration" (parseDataDecl <|> parseFunDecl)

parseDataDecl :: FParser Decl
parseDataDecl = annot "data declaration" $ do
  reserved "data"
  c <- parseConstrName
  tyVars <- P.many parseTyVarName
  reservedOp "="
  tys <- P.many parseTy
  pure (DeclData c tyVars tys)

parseFunDecl :: FParser Decl
parseFunDecl = annot "function declaration" $ do
  reserved "fun"
  name <- parseVarName
  reservedOp ":"
  ty <- parseTy
  reservedOp "="
  e <- parseExp
  pure (DeclFun name ty e)

parseTy :: FParser Ty
parseTy = P.buildExpressionParser table parseSimpleTy <?> "type"
  where
    table =
        [[P.Infix (reservedOp "->" >> pure TyArrow) P.AssocRight]
        ]
    parseSimpleTy = parseForall <|> parseTyVarOrConstr <|> parens parseTy
    parseTyVarOrConstr = do
      name <- parseNameInTy
      case name of
        Right a -> pure (TyVar a)
        Left c -> do
          case T.unpack (unConstrName c) of
            "Int" -> pure (TyPrim PrimInt)
            "Bool" -> pure (TyPrim PrimBool)
            "String" -> pure (TyPrim PrimString)
            "Char" -> pure (TyPrim PrimChar)
            "Void" -> pure (TyPrim PrimVoid)
            _ -> do
              args <- P.many parseSimpleTy
              pure (TyConstr c args)
    parseForall = do
      reserved "forall"
      as <- P.many parseTyVarName
      reservedOp "."
      ty <- parseTy
      pure (foldr (\a t -> TyForall a t) ty as)

parsePatClause :: FParser PatClause
parsePatClause = annot "pattern clause" $ do
  p <- parsePat
  reservedOp "->"
  e <- parseExp
  pure (PatClause p e)

parsePat :: FParser Pat
parsePat = annot "pattern" $ do
  name <- parseNameInExp
  case name of
    Left c -> do
      tys <- parseConstrTyArgs
      pats <- parseConstrPatArgs
      pure (PatConstr c tys pats)
    Right x -> do
      reservedOp ":"
      ty <- parseTy
      pure (if x == "_" then PatWild ty else PatVar x ty)
  where
    parseConstrTyArgs = do
      reservedOp "@"
      P.many parseTy
    parseConstrPatArgs :: FParser [Pat]
    parseConstrPatArgs = braces (P.sepBy parsePat comma)

parseAtomExp :: FParser (Either Ty Exp)
parseAtomExp = annot "atom exp" $
  parseInt <|> parseChar <|> parseString <|> parseTrue <|> parseFalse
  <|> parseVarOrConstr <|> parseTyAbs <|> parseAbs <|> parseTyInExp <|> parseIf
  <|> parseCase <|> parseParens
  where
    parseTyAbs = do
      reservedOp "\\\\"
      a <- parseTyVarName
      reservedOp "."
      e <- parseExp
      pureRight (ExpTyAbs a e)
    parseAbs = do
      reservedOp "\\"
      x <- parseVarName
      reservedOp ":"
      ty <- parseTy
      reservedOp "."
      e <- parseExp
      pureRight (ExpAbs x ty e)
    parseInt = do
      i <- P.natural lexer
      pureRight $ ExpInt i
    parseChar = do
      c <- charLiteral
      pureRight $ ExpChar c
    parseString = do
      s <- stringLiteral
      pureRight $ ExpStr s
    parseTrue = do
      reserved "True"
      pureRight $ ExpBool True
    parseFalse = do
      reserved "False"
      pureRight $ ExpBool False
    parseVarOrConstr = do
      name <- parseNameInExp
      case name of
        Right x -> pureRight (ExpVar x)
        Left c -> do
          tyArgs <- (parseConstrTyArgs <|> pure [])
          expArgs <- (parseConstrExpArgs <|> pure [])
          pureRight (ExpConstr c tyArgs expArgs)
    parseConstrTyArgs = do
      reservedOp "@"
      P.many parseTy
    parseConstrExpArgs :: FParser [Exp]
    parseConstrExpArgs = braces (P.sepBy parseExp comma)
    parseTyInExp = do
      reservedOp "@"
      ty <- parseTy
      pure (Left ty)
    parseIf = do
      reserved "if"
      e1 <- parseExp
      reserved "then"
      e2 <- parseExp
      reserved "else"
      e3 <- parseExp
      pureRight (ExpCond e1 e2 e3)
    parseCase = do
      reserved "case"
      e <- parseExp
      reserved "of"
      clauses <- P.semiSep1 lexer parsePatClause
      pureRight (ExpCase e clauses)
    parseParens = do
      e <- parens parseExp
      pureRight e
    pureRight x = pure (Right x)

parseAppExp :: FParser Exp
parseAppExp = do
  x <- P.chainl1 parseAtomExp (pure f)
  case x of
    Right e -> pure e
    Left ty ->
      fail ("Unexpected type in expression: " ++ prettyS ty)
  where
    f (Right e1) (Right e2) = Right (ExpApp e1 e2)
    f (Right e) (Left ty) = Right (ExpTyApp e ty)
    f (Left ty) _ = Left ty

parseExp :: FParser Exp
parseExp =
    P.buildExpressionParser table parseAppExp <?> "expression"
  where
    table =
        [
          [P.Prefix (reservedOp "-" >> pure (ExpUnOp Inv))],
          [binOpLeft "*" Mult, binOpLeft "/" Div, binOpLeft "%" Mod],
          [binOpLeft "+" Plus, binOpLeft "-" Minus],
          [binOp "==" Equal, binOp "!=" NotEqual],
          [binOp "<" Lt, binOp "<=" LtEqual, binOp ">" Gt, binOp ">=" GtEqual],
          [P.Prefix (reservedOp "!" >> pure (ExpUnOp Not))],
          [binOp "&&" And],
          [binOp "||" Or]
        ]
    binOpLeft :: T.Text -> BinOp -> P.Operator T.Text () (Reader ParserConfig) Exp
    binOpLeft n f = P.Infix (reservedOp n >> pure (\a b -> ExpBinOp a f b)) P.AssocLeft
    binOp :: T.Text -> BinOp -> P.Operator T.Text () (Reader ParserConfig) Exp
    binOp n f = P.Infix (reservedOp n >> return (\a b -> ExpBinOp a f b)) P.AssocNone

parseProg :: FParser Prog
parseProg = annot "program" $ do
  whiteSpace
  decls <- P.many parseDecl
  reserved "let"
  _ <- identifier
  reservedOp "="
  e <- parseExp
  pure (Prog decls e)

formatParseError :: T.Text -> P.ParseError -> T.Text
formatParseError input error =
    let pos = P.errorPos error
        line = P.sourceLine pos
        col = P.sourceColumn pos
        marker = T.replicate (col - 1) " " <> "^"
        msg =
            case drop (line - 1) (T.lines input) of
              [] -> T.pack (show error)
              (errLine:_) -> T.pack (show error) <> "\n\n" <> errLine <> "\n" <> marker <> "\n\n"
    in msg

runParserOnString :: FParser a -> T.Text -> Either P.ParseError a
runParserOnString p s =
    let p' = do
          x <- p
          P.eof
          pure x
        r = P.runParserT p' () "<input>" s
    in runReader r ()

runP :: FParser p -> T.Text -> Either T.Text p
runP parser src =
  case runParserOnString parser src of
    Right p -> Right p
    Left err -> Left (formatParseError src err)
