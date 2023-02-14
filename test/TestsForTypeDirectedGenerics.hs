{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module TestsForTypeDirectedGenerics (runAllTestsForTpypeDirectedGenerics) where

import Common.FGGParser
import Common.Utils
import TypeDirectedGeneric.SystemF.TopLevelTranslation
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL
import System.FilePath
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Text.Groom
import Text.Regex.TDFA hiding (match)
import Text.Printf

data TestSpec
  = TypecheckGood
  | TypecheckBad T.Text
  | EvalGood T.Text
  | EvalBad T.Text

parseTestSpec :: T.Text -> Either String TestSpec
parseTestSpec t =
  if | t == "TYPECHECK_GOOD" -> Right TypecheckGood
     | otherwise ->
       withArg EvalGood "EVAL_GOOD:" $
       withArg TypecheckBad "TYPECHECK_BAD:" $
       withArg EvalBad "EVAL_BAD:" $
       Left "invalid spec"
  where
    withArg :: (T.Text -> TestSpec)
            -> T.Text
            -> Either String TestSpec
            -> Either String TestSpec
    withArg f status next =
      case T.stripPrefix status t of
        Nothing -> next
        Just status -> Right (f (removeNoise status))

removeNoise :: T.Text -> T.Text
removeNoise (T.strip -> t) =
  let matches = map T.pack $ getAllTextMatches (T.unpack t =~ ("#<procedure:[^>]*>" :: String))
  in foldr (\m t -> T.replace m "#<procedure>" t) t matches

reportOk :: FilePath -> Int -> String -> IO ()
reportOk path idx msg =
  putStrLn $ printf "%3d OK %s: %s" idx path msg

reportError :: FilePath -> String -> [T.Text] -> IO ()
reportError path msg trace = do
  putStrLn ("ERROR " ++ path ++ ": " ++ msg)
  outputTrace trace
  fail "Test ERROR"

match :: T.Text -> T.Text -> Bool
match x t = x `T.isInfixOf` t

runTestForSpec :: FilePath -> Int -> TestSpec -> IO ()
runTestForSpec path idx spec = do
  let parseCfg =
        if "oopsla2022" `L.isInfixOf` path || "jfp2023" `L.isInfixOf` path
        then ParserConfig ModernGenerics
        else ParserConfig OldstyleGenerics
  prog <- parseFile path parseCfg
  let (result, trace) = runTranslation' prog
  case result of
    Left err ->
      case spec of
        TypecheckBad x ->
          if match x (T.pack err)
          then reportOk path idx "failed to typecheck with the expected error message"
          else reportError path
                   ("failed to typecheck with unexpected error.\n" ++
                    "ERROR:  " ++ err ++ "\n" ++
                    "EXPECT: " ++ T.unpack x)
                   trace
        _ ->
          reportError path ("failed to typecheck but should succeed") trace
    Right racketProg ->
      case spec of
        TypecheckBad _ ->
          reportError path ("typechecked but should fail") trace
        TypecheckGood ->
          reportOk path idx ("typechecked as expected")
        EvalGood r -> checkEval racketProg (Right r)
        EvalBad r -> checkEval racketProg (Left r)
  where
    checkEval :: TL.Prog -> Either T.Text T.Text -> IO ()
    checkEval racketProg expectedResult = do
      let outFile = "/tmp/fgg-target"
      writeFile (outFile ++ ".txt") (groom racketProg)
      result <- TL.evalProg (outFile ++ ".rkt") "" racketProg
      case (expectedResult, result) of
        (Right x, Right (removeNoise -> t)) ->
          if T.strip x == T.strip t
          then reportOk path idx ("evaluated successfully to the expected result")
          else reportError path
                 ("evaluated successfully but to an unexpected result.\n" ++
                  "RESULT: " ++ T.unpack t ++ "\n" ++
                  "EXPECT: " ++ T.unpack x)
                 []
        (Right _, Left err) ->
          reportError path ("evaluation failed but should succeed: " ++ T.unpack err) []
        (Left x, Left err) ->
          if match x err
          then reportOk path idx ("evaluation failed with the expected error message")
          else reportError path
                 ("evaluation failed but with an unexpected error.\n" ++
                  "ERROR:  " ++ T.unpack err ++ "\n" ++
                  "EXPECT: " ++ T.unpack x)
                 []
        (Left _, Right t) ->
          reportError path ("evaluation succeeded but should fail. Result: " ++ T.unpack t) []

runTest :: (FilePath, Int) -> IO ()
runTest (path, idx) = do
  src <- T.readFile path
  firstLine <- case T.lines src of
    [] -> fail ("File " ++ path ++ " is empty")
    (x:_) -> pure x
  case T.stripPrefix "/// TEST " (T.strip firstLine) of
    Nothing -> fail ("File " ++ path ++ " does not have to magic test header in the first line")
    Just spec ->
      case parseTestSpec spec of
        Left err -> fail ("Invalid test spec in first line of " ++ path ++ ": " ++ err)
        Right spec -> runTestForSpec path idx spec

runAllTestsForTpypeDirectedGenerics :: IO ()
runAllTestsForTpypeDirectedGenerics = do
  testFiles <-
    filter (\fp -> takeExtension fp `elem` [".fg", ".fgg"]) <$>
    traverseDir testDir
  when (length testFiles == 0) $
    fail ("No test files found in " ++ testDir)
  mapM_ runTest (zip testFiles [1..])
  where
    testDir = "test-files/type-directed-generics"
