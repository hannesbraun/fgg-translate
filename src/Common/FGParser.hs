{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Common.FGParser
    (
     parseFile, htf_thisModulesTests
    ) where

import Common.FGAST
import Common.Utils
import Common.Types

import Text.Parsec ( (<|>), (<?>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P

import Test.Framework

import Data.Maybe
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Extra
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import System.Directory

goLanguage :: P.GenLanguageDef T.Text () Identity
goLanguage =
    P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.reservedNames =
        ["import", "struct", "interface", "type", "func", "return", "package", "var"]
    , P.reservedOpNames = [".", "="]
    , P.identStart  = P.letter <|> P.char '_'
    , P.identLetter = P.alphaNum <|> P.oneOf "_'"
    , P.opStart = P.opLetter goLanguage
    , P.opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.caseSensitive = True
    }

annot :: T.Text -> GoParser a -> GoParser a
annot s p = p <?> T.unpack s

lexer :: P.GenTokenParser T.Text () Identity
lexer = P.makeTokenParser goLanguage

type GoParser = P.Parsec T.Text ()

symbol :: T.Text -> GoParser ()
symbol x = P.symbol lexer (T.unpack x) >>= const (pure ())

reserved :: T.Text -> GoParser ()
reserved t = P.reserved lexer (T.unpack t)

reservedOp :: T.Text -> GoParser ()
reservedOp t = P.reservedOp lexer (T.unpack t)

identifier :: GoParser T.Text
identifier = P.identifier lexer >>= (pure . T.pack)

parens :: GoParser a -> GoParser a
parens = P.parens lexer

braces :: GoParser a -> GoParser a
braces = P.braces lexer

comma :: GoParser ()
comma = P.comma lexer >> pure ()

semi :: GoParser ()
semi = P.semi lexer >> pure ()

stringLiteral :: GoParser T.Text
stringLiteral = P.stringLiteral lexer >>= (pure . T.pack)

charLiteral :: GoParser Char
charLiteral = P.charLiteral lexer

parseDecl :: GoParser Decl
parseDecl = (parseTypeDecl <|> parseMeDecl) <?> "declaration"

varIdent :: GoParser VarName
varIdent = identifier >>= pure . VarName

typeIdent :: GoParser TyName
typeIdent = identifier >>= pure . TyName

parseTypeDecl :: GoParser Decl
parseTypeDecl = annot "type declaration" $ do
  reserved "type"
  tyName <- typeIdent
  optTyLit <- P.optionMaybe parseTyLit -- starts with "struct" or "interface"
  case optTyLit of
    Just tyLit -> return $ Type tyName tyLit
    Nothing -> do
      exTyName <- typeIdent
      return $ Type tyName (TypeDecl exTyName)

parseVarDecl :: GoParser (VarName, TyName)
parseVarDecl = do
  x <- varIdent
  t <- typeIdent
  return (x, t)

parseMethodSpec :: GoParser MeSpec
parseMethodSpec = annot "method spec" $ do
  m <- identifier
  args <- parens $ P.sepBy parseVarDecl comma
  res <- typeIdent
  return $ MeSpec (MeName m)  (MeSig args res)

-- We split parsing method declarations in 2 parts so that we need a try only
-- for the first part, namely the "func (" part. The try is needed because
-- the main function declaration also starts with func.
-- Using try only for the 1st part improves error messages.
parseMeDecl :: GoParser Decl
parseMeDecl = do
  P.try parseMeDeclPart1
  parseMeDeclPart2

parseMeDeclPart1 :: GoParser()
parseMeDeclPart1 = annot "method declaration" $ do
  reserved "func"
  symbol "("

parseMeDeclPart2 :: GoParser Decl
parseMeDeclPart2 = annot "method declaration" $ do
  recv <- parseVarDecl
  symbol ")"
  spec <- parseMethodSpec
  expr <- braces $ do
    reserved "return"
    parseExpr
  return $ Method recv spec expr

parseTyLit :: GoParser TyLit
parseTyLit = parseStruct <|> parseIface

parseStruct :: GoParser TyLit
parseStruct = annot "struct literal" $ do
  reserved "struct"
  fields <- braces $ P.many $ do
    f <- identifier
    t <- typeIdent
    P.optional semi
    return (FieldName f, t)
  return (Struct fields)

parseIface :: GoParser TyLit
parseIface = annot "interface literal" $ do
  reserved "interface"
  specs <- braces $ P.many parseMethodSpec
  return (Iface specs)

data Term
    = TermInt Integer
    | TermChar Char
    | TermString T.Text
    | TermVar T.Text
    | TermMeCall MeName [Exp]
    | TermStruct TyName [Exp]
    | TermParens Exp
    | TermDot Term Term
    | TermQuestionMark Term Term
    | TermColon Term Term
    | TermBinOp BinOp Term Term
    | TermUnOp UnOp Term
      deriving (Show)

parsePrimTerm :: GoParser Term
parsePrimTerm = parseInt <|> parseChar <|> parseString <|> parseVarTerm <|> parseParensTerm
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
    -- x OR m(e...) OR t{e...}
    parseVarTerm = do
      x <- identifier
      args <- P.optionMaybe (P.try parseArgs)
      case args of
        Nothing -> return $ TermVar x
        Just (Left es) -> return $ TermMeCall (MeName x) es
        Just (Right es) -> return $ TermStruct (TyName x) es
    parseArgs =
        let p = P.sepBy parseExpr comma
        in (Left <$> parens p) <|> (Right <$> braces p)
    parseParensTerm =
        TermParens <$> parens parseExpr

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
          TermBinOp o l r -> do
                 l' <- termToExp l
                 r' <- termToExp r
                 return (BinOp o l' r')
          TermUnOp o t -> do
                 t' <- termToExp t
                 return (UnOp o t')
          TermVar x -> return (Var (VarName x))
          TermMeCall _m _es -> fail "function calls not supported"
          TermStruct ty es -> return $ StructLit ty es
          TermParens e -> return e
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
                   TermMeCall m es ->
                       return $ MeCall leftExpr m es
                   TermStruct _ty _es ->
                       fail "cannot use struct literal here"
                   TermParens (Var (VarName t)) ->
                       return $ TyAssert leftExpr (TyName t)
                   t -> fail ("cannot use " ++ show t ++ " here")

test_parseExpr :: IO ()
test_parseExpr = do
  subAssert $ testParse "foo" foo
  subAssert $ testParse "foo.m(bar, 2)" (MeCall foo m [bar, lit2])
  subAssert $ testParse "foo.m(bar.foo())" (MeCall foo m [MeCall bar (MeName "foo") []])
  subAssert $ testParse "foo.m(2).m(3)" (MeCall (MeCall foo m [lit2]) m [lit3])
  subAssert $ testParse "str { 2, 3 }" (StructLit str [lit2, lit3])
  subAssert $ testParse "str { foo.m(), 3 }" (StructLit str [MeCall foo m [], lit3])
  subAssert $ testParse "foo.f" (Select foo f)
  subAssert $ testParse "str{2,3}.f" (Select (StructLit str [lit2, lit3]) f)
  subAssert $ testParse "foo.m(2).m(3).f" (Select (MeCall (MeCall foo m [lit2]) m [lit3]) f)
  subAssert $ testParse "foo.(str)" (TyAssert foo str)
  subAssert $ testParse "foo. ( str)" (TyAssert foo str)
  subAssert $ testParse "foo.f.(str)" (TyAssert (Select foo f) str)
  subAssert $ testParse "! (x <= y + 4 && true == (x > y))"
                (let a = BinOp LtEqual x (BinOp Plus y (IntLit 4))
                     b = BinOp Equal true (BinOp Gt x y)
                 in UnOp Not (BinOp And a b))
  subAssert $ testParse "x < y ? foo.m(2) : 1"
                (let a = BinOp Lt x y
                     b = MeCall foo m [IntLit 2]
                 in Cond a b (IntLit 1))
  subAssert $ testParseError "foo.(x.f)"
  subAssert $ testParseError "foo.(x.m())"
  subAssert $ testParseError "m()"
  subAssert $ testParseError "foo.x{1}"
  where
    foo = Var (VarName "foo")
    x = Var (VarName "x")
    y = Var (VarName "y")
    true = Var (VarName "true")
    m = MeName "m"
    bar = Var (VarName "bar")
    lit2 = IntLit 2
    lit3 = IntLit 3
    str = TyName "str"
    f = FieldName "f"
    testParse s exp = do
        case runParserOnString parseExpr s of
          Left err -> assertFailure ("Unexpected parse error for " ++ show s ++ ": " ++ show err)
          Right exp' -> assertEqual exp exp'
    testParseError s =
        case runParserOnString parseExpr s of
          Left _ -> return ()
          Right exp ->
              assertFailure ("Expected parse error for input " ++ show s ++ " got: " ++ show exp)

parseMainFunc :: GoParser Main
parseMainFunc = annot "main function" $ do
  reserved "func"
  x <- identifier
  unless (x == "main") $ fail "Main function must be named 'main'"
  parens $ return ()
  braces $ do
    l <- P.many1 parseBinding
    printf <- P.optionMaybe parseExpr -- this is fmt.Printf("%v", result)
    case printf of
      Nothing -> pure ()
      Just (MeCall _ (MeName "Printf") _) -> pure ()
      Just _ ->
          fail ("Invalid main function: the main function must consist of " ++
                "a non-empty list of bindings 'var x = exp', followed by " ++
                "an optional fmt.Printf")
    return $ Main
        { m_bindings = L.init l
        , m_result = snd (L.last l)
        }
  where
    parseBinding = do
      reserved "var"
      x <- identifier
      reservedOp "="
      expr <- parseExpr
      return (VarName x, expr)

parseHeader :: GoParser ()
parseHeader = annot "header" $ do
  reserved "package"
  _ <- identifier
  _ <- P.many parseImport
  return ()
  where
    parseImport = do
      reserved "import"
      _ <- stringLiteral
      return ()

parseProg :: GoParser Program
parseProg = annot "program" $ do
  parseHeader
  decls <- P.many parseDecl
  expr <- parseMainFunc
  return $ Program decls expr

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

test_parseProg :: IO ()
test_parseProg = do
  subAssert $ testParse "package main func main() { var result = 1 }"
      (Program [] (Main [] (IntLit 1)))
  subAssert $ testParse myProgString myProg
  subAssert $ testParseError myProgStringError
  where
    myProg =
        Program
        { p_decls =
              [Type "I1"
                   (Iface
                    [MeSpec { ms_name = "foo"
                                         , ms_sig = MeSig [] "I2"}]
                   )
              , Type "T"
                    (Struct
                     [("f", "int")])
              , Method
                     ("t", "T")
                     (MeSpec { ms_name = "foo"
                                          , ms_sig = MeSig [] "I2" })
                     (Var "t")]
        , p_main =
            Main [
               ("x", StructLit "T" [IntLit 1])
              ]
              (MeCall
                  (MeCall (Var "x") "foo" [])
                  "bar"
                  [])
        }
    myProgString =
        T.unlines
        [ "package main"
        , ""
        , "import \"fmt\""
        , "type I1 interface {"
        , "    foo() I2"
        , "}"
        , "type T struct { f int }"
        , "func (t T) foo() I2 { return t }"
        , "func main() {"
        , "    var x = T{1}"
        , "    var result = x.foo().bar()"
        , "    fmt.Printf(\"%v\", result)"
        , "}"]
    myProgStringError =
        T.unlines
        [ "package main"
        , ""
        , "func main() {"
        , "    var y = true"
        , "    y.foo(y)"
        , "}"
        ]
    testParse s prog = do
        case runParserOnString parseProg s of
          Left err ->
              assertFailure ("Unexpected parse error:\n" ++ T.unpack (formatParseError s err))
          Right prog' -> assertEqual prog prog'
    testParseError s =
        case runParserOnString parseProg s of
          Left err ->
              if "Invalid main function" `L.isInfixOf` show err
                 then pure ()
                 else assertFailure ("Unexpected parse error: " ++ show err)
          Right p ->
              assertFailure ("Expected parse error for input " ++ show s ++ " got: " ++ show p)

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

runParserOnString :: GoParser a -> T.Text -> Either P.ParseError a
runParserOnString p s =
    let p' = do
          x <- p
          P.eof
          pure x
    in P.runParser p' () "<input>" s

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

parseFile' :: FilePath -> GoParser a -> IO (a, [Decl])
parseFile' fp parser = do
  s <- T.readFile fp
  let imports = findImports s
  resolvedImports <- mapM (resolveImport fp) imports
  decls <-
      flip concatMapM resolvedImports $ \imp -> do
          (ds1, ds2) <- parseFile' imp parseDeclsWithEof
          return (ds2 ++ ds1)
  -- TODO: cycle check
  let result = P.runParser parser () fp s
  case result of
    Right x -> return (x, decls)
    Left err ->
        let msg = formatParseError s err
        in fail (T.unpack msg)

-- fails if parsing fails
parseFile :: FilePath -> IO Program
parseFile fp = do
  (prog, decls) <- parseFile' fp parseProgWithEof
  return (Program (decls ++ p_decls prog) (p_main prog))
