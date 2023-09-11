{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module TestsForTypeDirectedGenerics (runAllTestsForTpypeDirectedGenerics) where

import Common.FGGParser
import Common.Utils
import TypeDirectedGeneric.Translation as Translation
import TypeDirectedGeneric.SystemF.TopLevelTranslation as SFTranslation
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL
import System.FilePath
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Text.Groom
import Text.Regex.TDFA hiding (match)
import Text.Printf

data TranslationType
  = Normal
  | SystemF

translationTypeAnnotation :: TranslationType -> String
translationTypeAnnotation transType = case transType of
    Normal -> ""
    SystemF -> " [System F]"

data TestSpec
  = TypecheckGood
  | TypecheckBad T.Text
  | EvalGood T.Text
  | EvalBad T.Text
  | Skip

parseTestSpec :: T.Text -> Either String TestSpec
parseTestSpec t =
  if | t == "TYPECHECK_GOOD" -> Right TypecheckGood
     | t == "SKIP" -> Right Skip
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

reportOk :: FilePath -> TranslationType -> Int -> String -> IO ()
reportOk path transType idx msg =
  putStrLn $ printf "%3d OK%s %s: %s" idx (translationTypeAnnotation transType) path msg

reportError :: FilePath -> TranslationType -> String -> [T.Text] -> IO ()
reportError path transType msg trace = do
  putStrLn ("ERROR" ++ (translationTypeAnnotation transType) ++ " " ++ path ++ ": " ++ msg)
  outputTrace trace
  fail "Test ERROR"

match :: T.Text -> T.Text -> Bool
match x t = x `T.isInfixOf` t

runTestForSpec :: FilePath -> Int -> TestSpec -> TranslationType -> IO ()
runTestForSpec _ _ Skip _ = pure ()
runTestForSpec path idx spec transType = do
  let parseCfg =
        if "oopsla2022" `L.isInfixOf` path || "jfp2023" `L.isInfixOf` path
        then ParserConfig ModernGenerics
        else ParserConfig OldstyleGenerics
  prog <- parseFile path parseCfg
  let (result, trace) = runTrans prog
  case result of
    Left err ->
      case spec of
        TypecheckBad x ->
          if match x (T.pack err)
          then reportOk path transType idx "failed to typecheck with the expected error message"
          else reportError path transType
                   ("failed to typecheck with unexpected error.\n" ++
                    "ERROR:  " ++ err ++ "\n" ++
                    "EXPECT: " ++ T.unpack x)
                   trace
        _ ->
          reportError path transType ("failed to typecheck but should succeed") trace
    Right racketProg ->
      case spec of
        TypecheckBad _ ->
          reportError path transType ("typechecked but should fail") trace
        TypecheckGood ->
          reportOk path transType idx ("typechecked as expected")
        EvalGood r -> checkEval racketProg (Right r)
        EvalBad r -> checkEval racketProg (Left r)
        Skip -> reportError path transType ("this test should have been skipped") trace
  where
    (runTrans, stdlib) = case transType of
      Normal -> (Translation.runTranslation', Translation.stdlibForTrans)
      SystemF -> (SFTranslation.runTranslation', "")
    checkEval :: TL.Prog -> Either T.Text T.Text -> IO ()
    checkEval racketProg expectedResult = do
      let outFile = "/tmp/fgg-target"
      writeFile (outFile ++ ".txt") (groom racketProg)
      result <- TL.evalProg (outFile ++ ".rkt") stdlib racketProg
      case (expectedResult, result) of
        (Right x, Right (removeNoise -> t)) ->
          if T.strip x == T.strip t
          then reportOk path transType idx ("evaluated successfully to the expected result")
          else reportError path transType
                 ("evaluated successfully but to an unexpected result.\n" ++
                  "RESULT: " ++ T.unpack t ++ "\n" ++
                  "EXPECT: " ++ T.unpack x)
                 []
        (Right _, Left err) ->
          reportError path transType ("evaluation failed but should succeed: " ++ T.unpack err) []
        (Left x, Left err) ->
          if match x err
          then reportOk path transType idx ("evaluation failed with the expected error message")
          else reportError path transType
                 ("evaluation failed but with an unexpected error.\n" ++
                  "ERROR:  " ++ T.unpack err ++ "\n" ++
                  "EXPECT: " ++ T.unpack x)
                 []
        (Left _, Right t) ->
          reportError path transType ("evaluation succeeded but should fail. Result: " ++ T.unpack t) []

runTest :: (FilePath, Int) -> IO ()
runTest (path, idx) = do
  src <- T.readFile path
  testLines <- case T.lines src of
    [] -> fail ("File " ++ path ++ " is empty")
    lines -> pure (take 2 lines)
  result <- mapM runTestForLine testLines
  case length (filter (\x -> x) result) of
    0 -> fail ("File " ++ path ++ " does not have a magic test header in the first lines")
    _ -> pure ()
  where
    runTestForLine :: T.Text -> IO Bool
    runTestForLine line =
      let rawSpec = case T.stripPrefix "/// TEST " (T.strip line) of
                      Nothing -> case T.stripPrefix "/// TEST_SYSTEMF " (T.strip line) of
                        Nothing -> Nothing
                        Just spec -> Just (SystemF, spec)
                      Just spec -> Just (Normal, spec)
      in  case rawSpec of
            Nothing -> pure False
            Just (transType, spec) -> case parseTestSpec spec of
              Left err -> fail ("Invalid test spec in test header of " ++ path ++ ": " ++ err)
              Right spec -> do
                runTestForSpec path idx spec transType
                pure True

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
