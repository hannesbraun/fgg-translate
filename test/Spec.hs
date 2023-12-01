{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Common.FGParser
import {-@ HTF_TESTS @-} Common.FGGParser
import {-@ HTF_TESTS @-} Common.FGGAST
import {-@ HTF_TESTS @-} TypeDirectedGeneric.UntypedTargetLanguage
import {-@ HTF_TESTS @-} TypeDirectedGeneric.SystemF.Typechecker
import {-@ HTF_TESTS @-} TypeDirectedGeneric.SystemF.Syntax
import {-@ HTF_TESTS @-} TypeDirectedGeneric.SystemF.Tests
import Common.Driver
import Common.Types
import TestsForTypeDirectedGenerics

import Test.Framework hiding (Args)
import Test.Framework.TestManager hiding (TestResult(..), TestOutput(..))
import System.Environment
import System.Exit
import System.Process
import Control.Monad
import qualified Data.Text as T
import qualified Data.List as L
import Data.String

data NeedCasts = NeedCasts | DontNeedCasts
               deriving (Eq, Show)

data TestOutput
    = TestOutputOk T.Text
    | Error
    deriving (Eq, Show)

instance IsString TestOutput where
    fromString s = TestOutputOk (T.pack s)

data TestSpec
    = TestSpec
    { ts_goFile :: FilePath
    , ts_output :: TestOutput
    , ts_needsCasts :: NeedCasts
    }
    deriving (Eq, Show)

fileTests :: [TestSpec]
fileTests =
    [
      TestSpec "test-files/haskell-syntax-directed/recursive-interfaces-1/Example.go" "Mk_T 1" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/recursive-interfaces-2/Example.go" "True" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/recursive-interfaces-3/Example.go" "Mk_T 42" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/recursive-interfaces-4/Example.go" "Mk_T 42" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/same-name-multiple-types/Example.go" "True" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/builtin-types/Example.go" "\"foo\"" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/structs/Example.go" "True" NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/simple-structural/Example.go" "1" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Example.go" "Mk_S 0 True" NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Fail1.go" Error NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Fail2.go" Error NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Fail3.go" Error NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Fail4.go" Error NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/casts/Fail5.go" Error NeedCasts
    , TestSpec "test-files/haskell-syntax-directed/iface-call/Example.go" "1" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/iface-field/Example.go" "1" DontNeedCasts
    , TestSpec "test-files/haskell-syntax-directed/typedefs/Example.go" "1" NeedCasts
    -- , TestSpec "test-files/conversion/Example.go" "1" NeedCasts
    -- , TestSpec "test-files/regular-expression/Example.go" "1" NeedCasts
    ]

runFileTest' :: TestSpec -> WithCasts -> IO ()
runFileTest' spec flag = do
  putStrLn ("\nRunning test for " ++ ts_goFile spec ++ " " ++ show flag)
  driverRun (Args (Just TargetSyntax) flag TraceOff (ts_goFile spec) OldstyleGenerics)
  let hsFile = outputFileFromInputFile ".hs" (ts_goFile spec) flag
  (ecode, out, err) <- readProcessWithExitCode "stack"
      ["exec", "ghci", "--", hsFile, "-e", "main"]
      ""
  case (ecode, ts_output spec) of
    (ExitFailure i, Error)
        | "<interactive>:" `L.isInfixOf` err ->
              putStrLn ("Running ghci failed with exit code " ++ show i ++ " as expected")
    (ExitFailure i, _) -> do
        putStrLn ("Running ghci failed unexpected with exit code " ++ show i)
        putStrLn ("Stdout:\n" ++ out)
        putStrLn ("Stderr:\n" ++ err)
        fail "ERROR"
    (ExitSuccess, Error) -> do
        putStrLn "Running ghci succeeded but should fail"
        fail "ERROR"
    (ExitSuccess, TestOutputOk expectedOut) -> do
        putStrLn "Running ghci succeeded"
        let tOut = T.strip (T.pack out)
        if tOut == expectedOut
           then putStrLn "Result ok"
           else do
             putStrLn ("Expected " ++ show (ts_output spec) ++ " as result, got " ++ show tOut)
             fail "ERROR"

runFileTest :: TestSpec -> IO ()
runFileTest spec = do
  runFileTest' spec WithCasts
  when (ts_needsCasts spec == DontNeedCasts) $
      runFileTest' spec WithoutCasts

main :: IO ()
main = do
  args <- getArgs
  let fast = fastOpt `elem` args
      hs = hsOpt `elem` args
      args' = filter (\x -> not (x `elem` customOpts)) args
  putStrLn "Running unit tests ..."
  ecode <- runTestWithArgs args' htf_importedTests
  case ecode of
    ExitFailure _ -> exitWith ecode
    ExitSuccess -> do
      unless fast $ do
        putStrLn "Running tests for type-directed translation with generics ..."
        runAllTestsForTpypeDirectedGenerics
        when hs $ do
          putStrLn "Running tests for syntax-directed translation ..."
          mapM_ runFileTest fileTests
  where
    fastOpt = "--fast"
    hsOpt = "--hs"
    customOpts = [fastOpt, hsOpt]
