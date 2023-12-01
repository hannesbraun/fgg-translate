module TypeDirectedGeneric.SystemF.TopLevelTranslation (runTranslation, runTranslation') where

import qualified Data.Text as T
import System.Exit
import System.IO

import qualified Common.FGGAST as G
import Common.Types
import Common.Utils
import TypeDirectedGeneric.TransCommon

import qualified TypeDirectedGeneric.Translation as TypeDirected
import qualified TypeDirectedGeneric.SystemF.Erasure as Erasure
import qualified TypeDirectedGeneric.SystemF.Translation as SystemFTrans
import qualified TypeDirectedGeneric.SystemF.Typechecker as Typechecker
import qualified TypeDirectedGeneric.UntypedTargetLanguage as UntypedTL

-- Translate Go to System F, then to Racket (while performing type erasure)
topLevelTranslate :: G.Program -> T UntypedTL.Prog
topLevelTranslate programGo = do
  programSystemF <- SystemFTrans.translateProgram programGo
  case Typechecker.runT (Typechecker.typeCheck programSystemF) of
    Left msg -> failT msg -- Typechecking failed in this case
    Right _ -> pure ()
  pure (Erasure.erase programSystemF)

runTrans :: G.Program -> (Either TransError UntypedTL.Prog, [T.Text])
runTrans goProgram =
  let config =
        TransConfig
          { tc_freshVarPrefix = T.pack "x-"
          , tc_assertSubType = \_ _ _ -> pure ()
          , tc_checkInst = \_ _ _ -> pure (Right ())
          }
   in genRunTrans config goProgram topLevelTranslate

runTranslation' :: G.Program -> (Either String UntypedTL.Prog, [T.Text])
runTranslation' p =
  case TypeDirected.runTrans p of
    (Left err, trace) -> (Left err, trace)
    (Right _, _) -> runTrans p

runTranslation :: TraceFlag -> T.Text -> FilePath -> G.Program -> IO T.Text
runTranslation traceFlag header filePath prog = do
  let (result, trace) = runTranslation' prog
  case traceFlag of
    TraceOn -> outputTrace trace
    TraceOff -> pure ()
  tProg <-
    case result of
      Right p -> pure p
      Left err -> do
        hPutStrLn stderr ("Typechecking " ++ filePath ++ " failed: " ++ err)
        exitWith (ExitFailure 1)
  pure (header <> UntypedTL.translateProg T.empty tProg)
