module TypeDirectedGeneric.SMLSharpTypedLambda.Translation (translateProgram) where

import TypeDirectedGeneric.TransCommon

import qualified TypeDirectedGeneric.SMLSharpTypedLambda.Language as SML
import qualified TypeDirectedGeneric.SystemF as SF

-- translateDeclaration :: SF.Decl -> T SML.TLDecl
-- translateDeclaration _ = failT "Internal error"

translateExpression :: SF.Exp -> T SML.TLExp
translateExpression (SF.ExpStr value) = pure (SML.TLString (SML.String value))
translateExpression (SF.ExpInt value) = pure (SML.TLInt (SML.Int64 (fromInteger value)))
translateExpression _ = failT "Internal error"

translateProgram :: SF.Prog -> T SML.Program
translateProgram (SF.Prog _declarations mainExpression) = do
  main <- translateExpression mainExpression
  pure (SML.Program main)
