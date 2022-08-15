{-# LANGUAGE OverloadedStrings #-}
module Common.Driver where

import qualified Common.FGParser as FgP
import qualified Common.FGGParser as FggP
import Common.Types
import Common.Utils
import qualified SyntaxDirected.Translation as S
import qualified TypeDirectedGeneric.Translation as Tg

import Data.Time.Clock
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Exception as E

data Target
    = TargetParse | TargetParseGeneric | TargetSyntaxAST | TargetSyntax
    | TargetType | TargetTypeGeneric
            deriving (Show)

targets :: String
targets = "'parse', 'parse-generic', 'syntax', 'type', 'type-generic'"

parseTarget :: ReadM Target
parseTarget =
    eitherReader $ \s ->
        case s of
          "parse" -> Right TargetParse
          "parse-generic" -> Right TargetParseGeneric
          "syntax" -> Right TargetSyntax
          "syntax-ast" -> Right TargetSyntaxAST
          "type" -> Right TargetType
          "type-generic" -> Right TargetTypeGeneric
          _ -> Left ("Unknown target. Supported: " ++ targets)

data Args
    = Args
    { a_target :: Maybe Target
    , a_casts :: WithCasts
    , a_trace :: TraceFlag
    , a_inputFile :: FilePath
    , a_genericsSyntax :: FggP.GenericsSyntax
    }
    deriving (Show)

outputFileFromInputFile :: String -> FilePath -> WithCasts -> FilePath
outputFileFromInputFile ext input wc =
    let suffix =
            case wc of
              WithCasts -> ext
              WithoutCasts -> "_NoCasts" ++ ext
    in dropExtension input ++ suffix

newtype Comment = Comment T.Text

autoHeader :: Comment -> T.Text
autoHeader (Comment comment) = comment <> " AUTOMATICALLY GENERATED "

canWrite :: FilePath -> Comment -> IO Bool
canWrite fp c = do
  b <- doesFileExist fp
  if b then checkContent else return True
  where
    checkContent = withFile fp ReadMode $ \h -> do
      autoGen <- loop h
      return autoGen
    loop h = do
      mLine <- tryGetLine h
      case mLine of
        Just t | autoHeader c `T.isInfixOf` t -> return True
               | otherwise -> loop h
        Nothing -> return False

runSyntaxDirectedAST :: Args -> IO ()
runSyntaxDirectedAST args = do
  goProg <- FgP.parseFile (a_inputFile args)
  putStrLn $ show $ S.runTrans (a_casts args) goProg

runTranslation :: Args -> String -> String -> Comment -> (T.Text -> IO T.Text) -> IO ()
runTranslation args name ext c run = do
  putStrLn ("Running " ++ name ++ " on " ++ a_inputFile args)
  t <- getCurrentTime
  let auto = autoHeader c <> showText t <> "\n"
  targetFile <- getTargetFile
  code <- run auto
  code <- E.evaluate code
  T.writeFile targetFile code
  putStrLn $ "Placed result of translation in " ++ targetFile
  where
    getTargetFile = do
      let targetFile1 = outputFileFromInputFile ext (a_inputFile args) (a_casts args)
      b1 <- canWrite targetFile1 c
      if b1
      then pure targetFile1
      else do
        let targetFile2 = outputFileFromInputFile ("_auto" ++ ext) (a_inputFile args) (a_casts args)
        b2 <- canWrite targetFile2 c
        if b2
        then pure targetFile2
        else
          fail ("Files " ++ targetFile1 ++ " and " ++ targetFile2 ++
                " exist and have not been generated by the translation. " ++
                "Refusing to overwrite!")


runSyntaxDirected :: Args -> IO ()
runSyntaxDirected args = do
  runTranslation args "syntax-directed translation" ".hs" (Comment "--") $ \auto -> do
    goProg <- FgP.parseFile (a_inputFile args)
    pure (S.translate (a_casts args) auto goProg)

runTypeDirectedGeneric :: Args -> IO ()
runTypeDirectedGeneric args = do
  runTranslation args "type-directed translation with generics" ".rkt" (Comment ";;") $ \auto -> do
    let parserCfg = FggP.ParserConfig (a_genericsSyntax args)
    goProg <- FggP.parseFile (a_inputFile args) parserCfg
    Tg.runTranslation (a_trace args) auto (a_inputFile args) goProg

driverRun :: Args -> IO ()
driverRun args = do
  target <- getTarget
  let parserCfg = FggP.ParserConfig (a_genericsSyntax args)
  case target of
    TargetParse -> do
      ast <- FgP.parseFile (a_inputFile args)
      putStrLn (show ast)
    TargetParseGeneric -> do
      ast <- FggP.parseFile (a_inputFile args) parserCfg
      putStrLn (show ast)
    TargetSyntax -> runSyntaxDirected args
    TargetSyntaxAST -> runSyntaxDirectedAST args
    TargetType -> fail "type-directed translation not implemented yet"
    TargetTypeGeneric -> runTypeDirectedGeneric args
  where
    getTarget =
        case a_target args of
          Just t -> pure t
          Nothing ->
              case takeExtension (a_inputFile args) of
                ".go" -> pure TargetSyntax
                ".fgg" -> pure TargetTypeGeneric
                _ -> fail ("No explicit --target given and there is no default for input file " ++
                           a_inputFile args)

parseArgs :: Parser Args
parseArgs =
  Args
  <$> (optional $ option parseTarget
       ( long "target"
       <> help ("The target (" ++ targets ++ ")")
       ))
  <*> ((\b -> if b then WithoutCasts else WithCasts) <$>
       switch (long "no-casts" <> help "Do not translate casts"))
  <*> ((\b -> if b then TraceOn else TraceOff) <$>
       switch (long "trace" <> help "Enable tracing"))
  <*> argument str (metavar "FILE")
  <*> ((\b -> if b then FggP.ModernGenerics else FggP.OldstyleGenerics) <$>
       switch (long "modern" <> help "Use modern syntax for generics"))

driverMain :: IO ()
driverMain = do
  args <- execParser opts
  driverRun args
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
      <> progDesc "Translations from Featherweight (Generic) Go to various target languages" )
