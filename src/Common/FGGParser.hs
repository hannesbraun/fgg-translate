{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Common.FGGParser
    (
     parseFile, ParserConfig(..), GenericsSyntax(..), htf_thisModulesTests
    ) where

import Common.FGGAST
import Common.Utils
import Common.Types

import Text.Parsec ( (<|>), (<?>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P

import Test.Framework

import Data.Maybe
import Data.Generics
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import System.Directory

goLanguage :: P.GenLanguageDef T.Text () (Reader ParserConfig)
goLanguage =
    P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.reservedNames =
        ["import", "struct", "interface", "type", "func", "return", "package", "var"
        , "true", "false"]
    , P.reservedOpNames = [".", "="]
    , P.identStart  = P.letter <|> P.char '_'
    , P.identLetter = P.alphaNum <|> P.oneOf "_'"
    , P.opStart = P.opLetter goLanguage
    , P.opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.caseSensitive = True
    }

annot :: T.Text -> GoParser a -> GoParser a
annot s p = p <?> T.unpack s

lexer :: P.GenTokenParser T.Text () (Reader ParserConfig)
lexer = P.makeTokenParser goLanguage

data GenericsSyntax = OldstyleGenerics | ModernGenerics
  deriving (Eq, Show)

data ParserConfig
  = ParserConfig
  { pc_genericsSyntax :: GenericsSyntax
  }
  deriving (Eq, Show)

oldstyleCfg :: ParserConfig
oldstyleCfg =
  ParserConfig
  { pc_genericsSyntax = OldstyleGenerics }

newstyleCfg :: ParserConfig
newstyleCfg =
  ParserConfig
  { pc_genericsSyntax = ModernGenerics }

type GoParser = P.ParsecT T.Text () (Reader ParserConfig)

switchTyParamStyle :: GoParser a -> GoParser a -> GoParser a
switchTyParamStyle old mod = do
  sty <- asks pc_genericsSyntax
  case sty of
    OldstyleGenerics -> old
    ModernGenerics -> mod

tyParamsDelims :: GoParser (GoParser a -> GoParser a)
tyParamsDelims = do
  sty <- asks pc_genericsSyntax
  case sty of
    OldstyleGenerics -> pure parens
    ModernGenerics -> pure brackets

symbol :: T.Text -> GoParser ()
symbol x = P.symbol lexer (T.unpack x) >>= const (pure ())

reserved :: T.Text -> GoParser ()
reserved t = P.reserved lexer (T.unpack t)

reservedOp :: T.Text -> GoParser ()
reservedOp t = P.reservedOp lexer (T.unpack t)

identifier :: GoParser T.Text
identifier = P.identifier lexer >>= (pure . T.pack)

whiteSpace :: GoParser ()
whiteSpace = P.whiteSpace lexer

parens :: GoParser a -> GoParser a
parens = P.parens lexer

brackets :: GoParser a -> GoParser a
brackets = P.brackets lexer

braces :: GoParser a -> GoParser a
braces = P.braces lexer

comma :: GoParser ()
comma = P.comma lexer >> pure ()

semi :: GoParser ()
semi = P.semi lexer >> pure ()

optSemi :: GoParser ()
optSemi = P.optional semi

stringLiteral :: GoParser T.Text
stringLiteral = P.stringLiteral lexer >>= (pure . T.pack)

charLiteral :: GoParser Char
charLiteral = P.charLiteral lexer

parseDecl :: GoParser Decl
parseDecl = annot "declaration" $ do
  d <- parseTypeDecl <|> parseFunctionOrMeDecl
  optSemi
  pure d

varIdent :: GoParser VarName
varIdent = identifier >>= pure . VarName

typeIdent :: GoParser TyName
typeIdent = identifier >>= pure . TyName

typeVar :: GoParser TyVarName
typeVar = identifier >>= pure . TyVarName

parseTyFormals :: GoParser TyFormals
parseTyFormals = annot "type formals" $ do
  mx <- P.optionMaybe (P.try parse)
  case mx of
    Just x -> return x
    Nothing -> return $ TyFormals []
  where
    parse = switchTyParamStyle parseOldstyle parseModern
    parseModern = brackets parseBody
    parseOldstyle = parens $ do
      reserved "type"
      parseBody
    parseBody = do
      l <- flip P.sepBy comma $ do
        a <- typeVar
        t <- P.optionMaybe parseType
        return (a, t)
      return (TyFormals l)

parseType :: GoParser Type
parseType = annot "type" $ do
  tyName <- typeIdent
  f <- tyParamsDelims
  optArgs <- P.optionMaybe $ f $ P.sepBy parseType comma
  let args = fromMaybe [] optArgs
  return (TyNamed tyName args)

parseTypeDecl :: GoParser Decl
parseTypeDecl = annot "type declaration" $ do
  reserved "type"
  tyName <- typeIdent
  tyFormals <- parseTyFormals
  optTyLit <- P.optionMaybe parseTyLit -- starts with "struct" or "interface"
  case optTyLit of
    Just tyLit -> return $ TypeDecl tyName tyFormals tyLit
    Nothing -> do
      exTy <- parseType
      return $ TypeDecl tyName tyFormals (TySyn exTy)

parseVarDecl :: GoParser (VarName, Type)
parseVarDecl = do
  x <- varIdent
  t <- parseType
  return (x, t)

parseMethodSpec :: GoParser MeSpec
parseMethodSpec = annot "method spec" $ do
  m <- identifier
  parseMethodSpec' (MeName m)

parseMethodSpec' :: MeName -> GoParser MeSpec
parseMethodSpec' name = annot "method spec" $ do
  formals <- parseTyFormals
  args <- parens $ P.sepBy parseVarDecl comma
  res <- P.optionMaybe parseType
  return $ MeSpec name (MeSig formals args (fromMaybe tyVoid res))

-- We split parsing method declarations in 2 parts so that we need a try only
-- for the first part, namely the "func (" part. The try is needed because
-- the main function declaration also starts with func.
-- Using try only for the 1st part improves error messages.
parseFunctionOrMeDecl :: GoParser Decl
parseFunctionOrMeDecl = parseMeDecl <|> parseFunDecl
  where
    parseMeDecl = do
      P.try parseMeDeclPart1
      parseMeDeclPart2
    parseFunDecl = do
      fname <- P.try parseFunDeclPart1
      parseFunDeclPart2 fname

parseMeDeclPart1 :: GoParser()
parseMeDeclPart1 = annot "method declaration" $ do
  reserved "func"
  symbol "("

parseMeDeclPart2 :: GoParser Decl
parseMeDeclPart2 = annot "method declaration" $ do
  var <- varIdent
  ty <- typeIdent
  formals <- parseTyFormals
  symbol ")"
  spec <- parseMethodSpec
  body <- braces parseMethodBody
  return $ MeDecl (var, ty, formals) spec body

parseMethodBody :: GoParser MeBody
parseMethodBody = annot "method/function body" $ do
  l <- P.many parseBindingOrExpr
  ret <- P.optionMaybe parseReturnExpr
  return $ MeBody
       { mb_bindings = l
       , mb_return = ret
       }
  where
    parseReturnExpr = do
      reserved "return"
      parseExpr
    parseBindingOrExpr = parseBinding <|> parseAnonBinding <|> parseToplevelExpr
    parseToplevelExpr = do
      e <- parseExpr
      return (VarName "_", Nothing, e)
    parseAnonBinding = do
      t <- P.try $ do
             x <- identifier
             unless (x == "_") $ fail "expected _"
             t <- P.optionMaybe parseType
             reservedOp "="
             pure t
      e <- parseExpr
      pure (VarName "_", t, e)
    parseBinding = do
      reserved "var"
      x <- identifier
      t <- P.optionMaybe parseType
      reservedOp "="
      expr <- parseExpr
      return (VarName x, t, expr)

parseFunDeclPart1 :: GoParser MeName
parseFunDeclPart1 = annot "function declaration" $ do
  reserved "func"
  x <- identifier
  return (MeName x)

parseFunDeclPart2 :: MeName -> GoParser Decl
parseFunDeclPart2 name = annot "method declaration" $ do
  spec <- parseMethodSpec' name
  body <- braces parseMethodBody
  return $ FunDecl spec body

parseTyLit :: GoParser TyLit
parseTyLit = parseStruct <|> parseIface

parseStruct :: GoParser TyLit
parseStruct = annot "struct literal" $ do
  reserved "struct"
  fields <- braces $ P.many $ do
    f <- identifier
    t <- parseType
    optSemi
    return (FieldName f, t)
  return (Struct fields)

parseIface :: GoParser TyLit
parseIface = annot "interface literal" $ do
  reserved "interface"
  specs <- braces $ P.many $ do
    spec <- parseMethodSpec
    optSemi
    return spec
  return (Iface specs)

data Term
    = TermInt Integer
    | TermChar Char
    | TermString T.Text
    | TermBool Bool
    | TermVar T.Text
    | TermMeCall MeName [Type] [Exp]
    | TermStruct Type [Exp]
    | TermParens (Either Exp Type)
    | TermDot Term Term
    | TermQuestionMark Term Term
    | TermColon Term Term
    | TermBinOp BinOp Term Term
    | TermUnOp UnOp Term
      deriving (Show)

parseTypeArgs :: GoParser [Type]
parseTypeArgs = do
  f <- tyParamsDelims
  liftM (fromMaybe []) $ annot "type arguments" $ P.optionMaybe $ f $ P.sepBy parseType comma

parsePrimTerm :: GoParser Term
parsePrimTerm =
  parseInt <|> parseChar <|> parseString <|> parseTrue <|> parseFalse
  <|> parseVarTerm <|> parseParensTerm
  where
    parseInt = do
      i <- P.integer lexer
      return $ TermInt i
    parseChar = do
      c <- charLiteral
      return $ TermChar c
    parseString = do
      s <- stringLiteral
      return $ TermString s
    parseTrue = do
      reserved "true"
      return $ TermBool True
    parseFalse = do
      reserved "false"
      return $ TermBool False
    -- x OR m(e...) OR t{e...}
    parseVarTerm = do
      x <- identifier
      args <- parseArgs
      case args of
        Nothing -> return $ TermVar x
        Just (ts, Left es) -> return $ TermMeCall (MeName x) ts es
        Just (ts, Right es) -> return $ TermStruct (TyNamed (TyName x) ts) es
    parseTermArgs =
        let p = P.sepBy parseExpr comma
        in (Left <$> parens p) <|> (Right <$> braces p)
    parseParensTerm =
        TermParens <$> (P.try (Left <$> parens parseExpr) <|> (Right <$> parens parseType))
    parseTypeArgsAndArgs = do
      tyArgs <- parseTypeArgs
      args <- parseTermArgs
      return $ Just (tyArgs, args)
    parseArgs :: GoParser (Maybe ([Type], Either [Exp] [Exp]))
    parseArgs =
          P.try parseTypeArgsAndArgs
      <|> P.try (parseTermArgs >>= \x -> return (Just ([], x)))
      <|> return Nothing

parseTerm :: GoParser Term
parseTerm =
    P.buildExpressionParser table parsePrimTerm <?> "expression"
  where
    table =
        [[P.Infix (reservedOp "." >> return TermDot) P.AssocLeft],
         [binOpLeft "*" Mult, binOpLeft "/" Div, binOpLeft "%" Mod],
         [binOpLeft "+" Plus, binOpLeft "-" Minus],
         [binOp "==" Equal, binOp "!=" NotEqual],
         [binOp "<" Lt, binOp "<=" LtEqual, binOp ">" Gt, binOp ">=" GtEqual],
         [P.Prefix (reservedOp "!" >> return (TermUnOp Not))],
         [binOp "&&" And],
         [binOp "||" Or],
         [P.Infix (reservedOp ":" >> return TermColon) P.AssocNone],
         [P.Infix (reservedOp "?" >> return TermQuestionMark) P.AssocNone]
        ]
    binOpLeft n f = P.Infix (reservedOp n >> return (TermBinOp f)) P.AssocLeft
    binOp n f = P.Infix (reservedOp n >> return (TermBinOp f)) P.AssocNone

parseExpr :: GoParser Exp
parseExpr = annot "expression" $ do
  t <- parseTerm
  termToExp t
  where
    termToExp t =
        case t of
          TermInt i -> return (IntLit i)
          TermChar c -> return (CharLit c)
          TermString s -> return (StrLit s)
          TermBool b -> return (BoolLit b)
          TermBinOp o l r -> do
              l' <- termToExp l
              r' <- termToExp r
              return (BinOp o l' r')
          TermUnOp o t -> do
              t' <- termToExp t
              return (UnOp o t')
          TermVar x -> return (Var (VarName x))
          TermMeCall f ts es -> return $ FunCall f ts es
          TermStruct ty es -> return $ StructLit ty es
          TermParens (Left e) -> return e
          TermParens (Right t) -> fail ("unexpected type " ++ show t)
          TermQuestionMark l r -> do
              cond <- termToExp l
              case r of
                TermColon trueExp falseExp -> do
                    trueExp' <- termToExp trueExp
                    falseExp' <- termToExp falseExp
                    return $ Cond cond trueExp' falseExp'
                t -> fail ("cannot use " ++ show t ++ " here")
          TermColon _ _ -> fail ("cannot use " ++ show t ++ " here")
          TermDot leftTerm rightTerm -> do
             leftExpr <- termToExp leftTerm
             case rightTerm of
               TermVar x ->
                   return $ Select leftExpr (FieldName x)
               TermMeCall m ts es ->
                   return $ MeCall leftExpr m ts es
               TermStruct _ty _es ->
                   fail "cannot use struct literal here"
               TermParens (Left e) -> do
                   t <- exprToType e
                   return $ TyAssert leftExpr t
               TermParens (Right t) -> return $ TyAssert leftExpr t
               t -> fail ("cannot use " ++ show t ++ " here")
    exprToType e =
        case e of
          Var (VarName t) -> return $ TyNamed (TyName t) []
          FunCall (MeName f) [] es -> do
              ts <- mapM exprToType es
              return $ TyNamed (TyName f) ts
          _ -> fail ("invalid type: " ++ show e)

test_parseExpr :: IO ()
test_parseExpr = do
  subAssert $ testParse "foo" foo
  subAssert $ testParse "foo.m(bar, 2)" (MeCall foo m [] [bar, lit2])
  subAssert $ testParse "m()" (FunCall m [] [])
  subAssert $ testParse "m(bar, 2)" (FunCall m [] [bar, lit2])
  subAssert $ testParse "foo.m()(bar, 2)" (MeCall foo m [] [bar, lit2])
  subAssert $ testParse "foo.m(Int)(bar, 2)" (MeCall foo m [tInt] [bar, lit2])
  subAssert $ testParse "m(Int)(bar, 2)" (FunCall m [tInt] [bar, lit2])
  subAssert $ testParse "foo.m(Int,Bool)(bar, 2)" (MeCall foo m [tInt, tBool] [bar, lit2])
  subAssert $ testParse "foo.m(Pair(Int,Bool), Bool)(bar, 2)"
    (MeCall foo m [tPair tInt tBool, tBool] [bar, lit2])
  subAssert $ testParse "foo.m(bar.foo())"
    (MeCall foo m [] [MeCall bar (MeName "foo") [] []])
  subAssert $ testParse "foo.m(2).m(3)" (MeCall (MeCall foo m [] [lit2]) m [] [lit3])
  subAssert $ testParse "str { 2, 3 }" (StructLit str [lit2, lit3])
  subAssert $ testParse "str { foo.m(), 3 }" (StructLit str [MeCall foo m [] [], lit3])
  subAssert $ testParse "foo.f" (Select foo f)
  subAssert $ testParse "str{2,3}.f" (Select (StructLit str [lit2, lit3]) f)
  subAssert $ testParse "foo.m(2).m(3).f"
    (Select (MeCall (MeCall foo m [] [lit2]) m [] [lit3]) f)
  subAssert $ testParse "foo.(str)" (TyAssert foo str)
  subAssert $ testParse "foo. ( str)" (TyAssert foo str)
  subAssert $ testParse "foo.f.(str)" (TyAssert (Select foo f) str)
  subAssert $ testParse "! (x <= y + 4 && true == (x > y))"
                (let a = BinOp LtEqual x (BinOp Plus y (IntLit 4))
                     b = BinOp Equal true (BinOp Gt x y)
                 in UnOp Not (BinOp And a b))
  subAssert $ testParse "x < y ? foo.m(2) : 1"
                (let a = BinOp Lt x y
                     b = MeCall foo m [] [IntLit 2]
                 in Cond a b (IntLit 1))
  subAssert $ testParse "foo.(Foo(Int, Bool))"
                (TyAssert foo (TyNamed (TyName "Foo")
                                       [TyNamed "Int" [], TyNamed "Bool" []]))
  subAssert $ testParseModern "foo.(Foo[Int, Bool])"
                (TyAssert foo (TyNamed (TyName "Foo")
                                       [TyNamed "Int" [], TyNamed "Bool" []]))
  subAssert $ testParseError "foo.(x.f)"
  subAssert $ testParseError "foo.(x.m())"
  subAssert $ testParseError "foo.x{1}"
  where
    foo = Var (VarName "foo")
    x = Var (VarName "x")
    y = Var (VarName "y")
    true = BoolLit True
    m = MeName "m"
    bar = Var (VarName "bar")
    lit2 = IntLit 2
    lit3 = IntLit 3
    str = TyNamed (TyName "str") []
    f = FieldName "f"
    tInt = TyNamed (TyName "Int") []
    tBool = TyNamed (TyName "Bool") []
    tPair a b = TyNamed (TyName "Pair") [a, b]
    testParseGen cfg s exp = do
        case runParserOnString cfg parseExpr s of
          Left err -> assertFailure ("Unexpected parse error for " ++ show s ++ ": " ++ show err)
          Right exp' -> assertEqual exp exp'
    testParse = testParseGen oldstyleCfg
    testParseModern = testParseGen newstyleCfg
    testParseError s =
        case runParserOnString oldstyleCfg parseExpr s of
          Left _ -> return ()
          Right exp ->
              assertFailure ("Expected parse error for input " ++ show s ++ " got: " ++ show exp)

parseHeader :: GoParser ()
parseHeader = annot "header" $ do
  reserved "package"
  _ <- identifier
  optSemi
  _ <- P.many parseImport
  return ()
  where
    parseImport = do
      reserved "import"
      _ <- stringLiteral
      optSemi
      return ()

predefinedTypes :: [TyName]
predefinedTypes = map TyName ["int", "bool", "string"]

parseProg :: GoParser Program
parseProg = annot "program" $ do
  whiteSpace
  _ <- P.optionMaybe parseHeader
  allDecls <- P.many parseDecl
  let mains = mapMaybe asMain allDecls
      decls = mapMaybe asDecl allDecls
  pure $ Program decls mains
  where
    asMain (FunDecl (MeSpec name (MeSig (TyFormals []) [] resTy)) body)
      | isMainFunName name && isVoid resTy = Just body
    asMain _ = Nothing
    asDecl d =
      case asMain d of
        Just _ -> Nothing
        Nothing -> Just d
    isMainFunName :: MeName -> Bool
    isMainFunName (MeName name) =
      name `elem` ("main" : map T.pack altMains)
      where
        altMains = ["main" ++ show i | i <- [0..10]]

parseDeclsWithEof :: GoParser [Decl]
parseDeclsWithEof = do
  parseHeader
  decls <- P.many parseDecl
  P.eof
  return decls

parseProgWithEof :: GoParser Program
parseProgWithEof = do
  p <- parseProg
  P.eof
  return p

test_parseMethodBody :: IO ()
test_parseMethodBody = do
  subAssert $ testParse "_ = doWork()"
    (MeBody{mb_bindings =
         [(VarName{unVarName = "_"}, Nothing,
           FunCall (MeName{unMeName = "doWork"}) [] [])],
       mb_return = Nothing})
  where
    testParse s expected =
      case runParserOnString newstyleCfg parseMethodBody s of
        Left err -> assertFailure ("Unexpected parse error for " ++ show s ++ ": " ++ show err)
        Right res -> assertEqual expected res

test_parseProg :: IO ()
test_parseProg = do
  subAssert $ testParse "package main func main() { var result = 1 }"
    (Program
      [anyDecl]
      [MeBody [(VarName "result", Nothing, IntLit 1)] Nothing])
  subAssert $ testParse myProgString myProg
  subAssert $ testParse myProgStringGeneric myProgGeneric
  subAssert $ testParse' ModernGenerics myModernProgStringGeneric myProgGeneric
  where
    anyDecl = TypeDecl "Any" (TyFormals []) (Iface [])
    myProg =
        Program
        { p_decls =
              [ anyDecl
              , TypeDecl "I1" noTyFormals
                   (Iface
                    [MeSpec {
                       ms_name = "foo"
                     , ms_sig = MeSig noTyFormals [] (TyNamed "I1" [])}]
                   )
              , TypeDecl "T" noTyFormals
                    (Struct
                     [("f", TyNamed "int" [])])
              , MeDecl
                     ("t", "T", noTyFormals)
                     (MeSpec {
                        ms_name = "foo"
                      , ms_sig = MeSig noTyFormals [] (TyNamed "I1" []) })
                     (MeBody [] (Just $ Var "t"))]
        , p_mains =
            [
              MeBody {
                mb_bindings =
                    [(VarName{unVarName = "x"}, Nothing,
                      StructLit (TyNamed (TyName{unTyName = "T"}) []) [IntLit 1]),
                      (VarName{unVarName = "result"}, Nothing,
                       MeCall
                        (MeCall (Var (VarName{unVarName = "x"})) (MeName{unMeName = "foo"})
                          []
                          [])
                        (MeName{unMeName = "bar"})
                        []
                        []),
                      (VarName "_", Nothing,
                       MeCall (Var (VarName{unVarName = "fmt"}))
                       (MeName{unMeName = "Printf"})
                       []
                       [StrLit "%v", Var (VarName{unVarName = "result"})])
                    ],
               mb_return = Nothing }]
        }
    myProgString =
        T.unlines
        [ "package main"
        , ""
        , "import \"fmt\""
        , "type I1(type) interface {"
        , "    foo() I1"
        , "}"
        , "type T struct { f int }"
        , "func (t T) foo() I1 { return t }"
        , "func main() {"
        , "    var x = T{1}"
        , "    var result = x.foo().bar()"
        , "    fmt.Printf(\"%v\", result)"
        , "}"]
    myProgGeneric =
        Program
        { p_decls =
              [ anyDecl
              , TypeDecl "I1" (TyFormals [(TyVarName "a", Nothing)])
                   (Iface
                    [MeSpec {
                       ms_name = "foo"
                     , ms_sig = MeSig (TyFormals [(TyVarName "c", Nothing)])
                                  [] (TyNamed "I1" [])}]
                   )
              , TypeDecl "T" (TyFormals
                                [ (TyVarName "a", Nothing)
                                , (TyVarName "b",
                                    Just (TyNamed (TyName "I1")
                                                     [TyVar (TyVarName "a")]))])
                    (Struct
                     [("f", TyVar (TyVarName "a"))])
              , FunDecl
                     (MeSpec {
                        ms_name = "bar"
                      , ms_sig =
                          MeSig (TyFormals [(TyVarName "a", Nothing)])
                            [(VarName "x", TyVar (TyVarName "a"))]
                            (TyVar (TyVarName "a")) })
                     (MeBody [] (Just $ Var "x"))
              , MeDecl
                     ("t", "T", TyFormals [(TyVarName "a", Nothing)])
                     (MeSpec {
                        ms_name = "foo"
                      , ms_sig =
                          MeSig (TyFormals [(TyVarName "c", Nothing)]) []
                            (TyVar (TyVarName "a")) })
                     (MeBody [] (Just $ Var "t"))
              ]
        , p_mains =
            [
              MeBody {
                mb_bindings =
                    [(VarName{unVarName = "x"}, Nothing,
                      StructLit (TyNamed (TyName{unTyName = "T"}) []) [IntLit 1]),
                     (VarName{unVarName = "result"}, Nothing,
                      MeCall
                       (MeCall (Var (VarName{unVarName = "x"})) (MeName{unMeName = "foo"})
                         []
                         [])
                       (MeName{unMeName = "bar"})
                       []
                       []),
                      (VarName "_", Nothing,
                       MeCall (Var (VarName{unVarName = "fmt"}))
                       (MeName{unMeName = "Printf"})
                       []
                       [StrLit "%v", Var (VarName{unVarName = "result"})])
                    ],
                  mb_return = Nothing }]
        }
    myProgStringGeneric =
        T.unlines
        [ "package main"
        , ""
        , "import \"fmt\""
        , "type I1(type a) interface {"
        , "    foo(type c)() I1"
        , "}"
        , "type T(type a, b I1(a)) struct { f a }"
        , "func bar(type a)(x a) a { return x }"
        , "func (t T(type a)) foo(type c)() a { return t }"
        , "func main() {"
        , "    var x = T{1}"
        , "    var result = x.foo().bar()"
        , "    fmt.Printf(\"%v\", result)"
        , "}"]
    myModernProgStringGeneric =
        T.unlines
        [ "package main"
        , ""
        , "import \"fmt\""
        , "type I1[a] interface {"
        , "    foo[c]() I1"
        , "}"
        , "type T[a, b I1[a]] struct { f a }"
        , "func bar[a](x a) a { return x }"
        , "func (t T[a]) foo[c]() a { return t }"
        , "func main() {"
        , "    var x = T{1}"
        , "    var result = x.foo().bar()"
        , "    fmt.Printf(\"%v\", result)"
        , "}"]
    testParse = testParse' OldstyleGenerics
    testParse' genericsStyle s prog = do
        let parseCfg = ParserConfig genericsStyle
        case runParserOnString parseCfg parseProg s of
          Left err ->
              assertFailure ("Unexpected parse error:\n" ++ T.unpack (formatParseError s err))
          Right (fixProgram -> prog') -> assertEqual prog prog'

noTyFormals :: TyFormals
noTyFormals = TyFormals []

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

runParserOnString :: ParserConfig -> GoParser a -> T.Text -> Either P.ParseError a
runParserOnString cfg p s =
    let p' = do
          x <- p
          P.eof
          pure x
        r = P.runParserT p' () "<input>" s
    in runReader r cfg

findImports :: T.Text -> [FilePath]
findImports t =
    mapMaybe findImport (T.lines t)
  where
    findImport l =
        case stripPrefix "//#import " l of
          Just rest -> Just (T.unpack (T.strip rest))
          Nothing -> Nothing

resolveImport :: FilePath -> FilePath -> IO FilePath
resolveImport main imp = do
  let relPath = takeDirectory main </> imp
  b <- doesFileExist relPath
  unless b $ fail
      ("Cannot resolve import " ++ imp ++ " in " ++ main ++ ": file " ++ relPath ++
       " does not exist")
  canonicalizePath relPath

parseFile' :: FilePath -> ParserConfig -> GoParser a -> IO (a, [Decl])
parseFile' fp cfg parser = do
  s <- T.readFile fp
  let imports = findImports s
  resolvedImports <- mapM (resolveImport fp) imports
  decls <-
      flip concatMapM resolvedImports $ \imp -> do
          (ds1, ds2) <- parseFile' imp cfg parseDeclsWithEof
          return (ds2 ++ ds1)
  -- TODO: cycle check
  let result = runReader (P.runParserT parser () fp s) cfg
  case result of
    Right x -> return (x, decls)
    Left err ->
        let msg = formatParseError s err
        in fail (T.unpack msg)

fixProgram :: Program -> Program
fixProgram progWithoutAny =
  let prog = Program (addAnyToDecls (p_decls progWithoutAny)) (p_mains progWithoutAny)
      definedTypes = Set.fromList (predefinedTypes ++ mapMaybe definesType (p_decls prog))
  in everywhere (mkT (rewriteType definedTypes)) prog
  where
    definesType decl =
        case decl of
          TypeDecl tyName _ _ -> Just tyName
          _ -> Nothing
    rewriteType :: Set.Set TyName -> Type -> Type
    rewriteType tyNames ty =
        case ty of
          TyNamed t []
              | not (t `Set.member` tyNames) -> TyVar (TyVarName (unTyName t))
          _ -> ty
    addAnyToDecls decls =
      case L.find isDeclForAny decls of
        Just _ -> decls
        Nothing -> anyDecl : decls
      where
        isDeclForAny decl =
          case decl of
            TypeDecl t _ _ -> t == anyName
            _ -> False
        anyName = TyName "Any"
        anyDecl = TypeDecl anyName (TyFormals []) (Iface [])

-- fails if parsing fails
parseFile :: FilePath -> ParserConfig -> IO Program
parseFile fp cfg = do
  (prog, decls) <- parseFile' fp cfg parseProgWithEof
  return $ fixProgram (Program (decls ++ p_decls prog) (p_mains prog))
