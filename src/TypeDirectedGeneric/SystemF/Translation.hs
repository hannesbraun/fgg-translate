{-# LANGUAGE OverloadedStrings #-}

module TypeDirectedGeneric.SystemF.Translation (translateProgram) where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Common.FGGAST as G
import Common.PrettyUtils
import Common.Types
import TypeDirectedGeneric.TransCommon

import qualified TypeDirectedGeneric.SystemF as TL

structPrefix :: T.Text
structPrefix = T.pack "Struct"

interfacePrefix :: T.Text
interfacePrefix = T.pack "Interface"

typePrefix :: G.TyLit -> T.Text
typePrefix (G.Struct _) = structPrefix
typePrefix (G.TySyn _) = T.pack "TypeSynonym"
typePrefix (G.Iface _) = interfacePrefix

tupleName :: Int -> TL.ConstrName
tupleName i = TL.ConstrName $ T.pack $ "Tuple" ++ show i

tupleTyVarName :: Int -> TL.TyVarName
tupleTyVarName i = TL.TyVarName $ T.pack $ "T" ++ show i

generateGenericTuples :: Int -> [TL.Decl]
generateGenericTuples max = map (\i -> TL.DeclData (tupleName i) (map tupleTyVarName [1 .. i]) (map (\j -> TL.TyVar (tupleTyVarName j)) [1 .. i])) [0 .. max]

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

translateDeclaration :: G.Decl -> T [TL.Decl]
translateDeclaration (G.TypeDecl (G.TyName name) formals typeLiteral) = withCtx ("translation of type declaration " ++ prettyS name) $ do
  let tyEnv = tyFormalsToTyEnv formals
  let translatedFormals = translateTyVarNames formals
  (translatedTypes, additionalDeclarations) <- translateTypeLiteral tyEnv formals typeLiteral name
  let declarations =
        TL.DeclData (TL.ConstrName (typePrefix typeLiteral <> name)) translatedFormals translatedTypes
          : additionalDeclarations
  pure declarations
translateDeclaration (G.FunDecl methodSpec methodBody) =
  withCtx ("translation of function declaration " ++ (prettyS $ G.ms_name methodSpec)) $
    translateMethodDeclaration Nothing methodSpec methodBody
translateDeclaration (G.MeDecl receiver methodSpec methodBody) =
  withCtx
    ( "translation of method declaration "
        ++ (prettyS $ G.ms_name methodSpec)
        ++ " for receiver "
        ++ (prettyS $ snd3 receiver)
    )
    $ translateMethodDeclaration (Just receiver) methodSpec methodBody

translateMethodDeclaration :: Maybe (G.VarName, G.TyName, G.TyFormals) -> G.MeSpec -> G.MeBody -> T [TL.Decl]
translateMethodDeclaration receiver methodSpec methodBody = do
  tyEnv <- pure $ case receiver of
    Just (_, _, receiverFormals) -> tyFormalsToTyEnv receiverFormals
    Nothing -> emptyTyEnv
  tyEnv <- pure $ joinTyEnvs tyEnv $ tyFormalsToTyEnv $ G.msig_tyArgs $ G.ms_sig methodSpec
  varEnv <- mkVarEnv $ G.msig_args $ G.ms_sig methodSpec
  varEnv <- case receiver of
    Just (receiverName, receiverTypeName, receiverFormals) -> do
      let receiverTypeArgs = map (\(tyVar, _) -> G.TyVar tyVar) (G.unTyFormals receiverFormals)
      extendVarEnv varEnv [(receiverName, G.TyNamed receiverTypeName receiverTypeArgs)]
    Nothing -> pure varEnv
  (bodyType, translatedBody) <- translateMethodBody varEnv tyEnv methodBody
  -- coerce return type to required type
  translatedBody <- generateCoercion tyEnv (bodyType, translatedBody) (G.msig_res $ G.ms_sig methodSpec)
  translatedAbstractionExpression <- buildAbstractionExpression tyEnv translatedBody receiver methodSpec
  translatedTypeInfo <- translateMethodSpec tyEnv receiver methodSpec
  name <- pure $ G.unMeName $ G.ms_name methodSpec
  name <- pure $ case receiver of
    Just r -> name <> "_" <> (G.unTyName $ snd3 r)
    Nothing -> name
  pure [TL.DeclFun (TL.VarName name) translatedTypeInfo translatedAbstractionExpression]

translateTyVarNames :: G.TyFormals -> [TL.TyVarName]
translateTyVarNames formals = map (\formal -> TL.TyVarName $ G.unTyVarName (fst formal)) (G.unTyFormals formals)

translateTypeLiteral :: TyEnv -> G.TyFormals -> G.TyLit -> T.Text -> T ([TL.Ty], [TL.Decl])
translateTypeLiteral tyEnv _ (G.Struct fields) _ = do
  translatedTypes <- mapM (\field -> translateType tyEnv (snd field)) fields
  pure (translatedTypes, [])
translateTypeLiteral _ _ (G.TySyn _) _ = do
  failT "Type synonyms are not supported as of now"
translateTypeLiteral tyEnv _formals (G.Iface methods) _name = do
  translatedMethods <- mapM (translateMethodSpec tyEnv Nothing) methods
  pure (translatedMethods, [])

translateType :: TyEnv -> G.Type -> T TL.Ty
translateType tyEnv goType = do
  assertTypeOk tyEnv goType
  classified <- classifyTy goType
  case classified of
    TyKindBuiltin TyInt -> pure $ TL.TyPrim TL.PrimInt
    TyKindBuiltin TyRune -> pure $ TL.TyPrim TL.PrimInt
    TyKindBuiltin TyString -> pure $ TL.TyPrim TL.PrimString
    TyKindBuiltin TyBool -> pure $ TL.TyPrim TL.PrimBool
    TyKindBuiltin TyVoid -> pure $ TL.TyPrim TL.PrimVoid
    TyKindTyVar (G.TyVarName name) -> pure $ TL.TyVar $ TL.TyVarName name
    TyKindStruct tyName tyArgs -> do
      translatedTyArgs <- mapM (translateType tyEnv) tyArgs
      let name = G.unTyName tyName
      pure $ TL.TyConstr (TL.ConstrName $ structPrefix <> name) translatedTyArgs
    TyKindIface tyName tyArgs -> do
      translatedTyArgs <- mapM (translateType tyEnv) tyArgs
      let name = G.unTyName tyName
      pure $ TL.TyConstr (TL.ConstrName $ interfacePrefix <> name) translatedTyArgs

translateMethodSpec :: TyEnv -> Maybe (G.VarName, G.TyName, G.TyFormals) -> G.MeSpec -> T TL.Ty
translateMethodSpec oldTyEnv receiver (G.MeSpec _meName (G.MeSig formals args resultType)) = do
  let tyEnv = joinTyEnvs oldTyEnv $ tyFormalsToTyEnv formals
  translatedResultType <- translateType tyEnv resultType
  translatedFunArgs <- translateFunctionArguments tyEnv args
  let translatedTypeWithArgs = TL.TyArrow translatedFunArgs translatedResultType
  translatedCoercionArgs <- translateCoercionArguments tyEnv formals
  let translatedTypeWithCoercions = TL.TyArrow translatedCoercionArgs translatedTypeWithArgs
  let translatedTypeWithTypeAbstractions = translateTypeAbstractions formals translatedTypeWithCoercions
  case receiver of
    Nothing -> pure translatedTypeWithTypeAbstractions
    Just (_, receiverType, receiverTypeArgs) -> do
      let tyVars = map (\(tyVar, _) -> G.TyVar tyVar) (G.unTyFormals receiverTypeArgs)
      translatedReceiverType <- translateType tyEnv (G.TyNamed receiverType tyVars)
      let translatedTypeWithReceiver = TL.TyArrow translatedReceiverType translatedTypeWithTypeAbstractions
      translatedReceiverCoercionArgs <- translateCoercionArguments tyEnv receiverTypeArgs
      let translatedTypeWithRecvCoercions = TL.TyArrow translatedReceiverCoercionArgs translatedTypeWithReceiver
      let translatedTypeWithRecvTypeAbstractions = translateTypeAbstractions receiverTypeArgs translatedTypeWithRecvCoercions
      pure translatedTypeWithRecvTypeAbstractions

translateFunctionArguments :: TyEnv -> [(G.VarName, G.Type)] -> T TL.Ty
translateFunctionArguments tyEnv args = do
  translatedTypes <- mapM (translateType tyEnv . snd) args
  let argConstrName = tupleName $ length args
  pure $ TL.TyConstr argConstrName translatedTypes

translateCoercionArguments :: TyEnv -> G.TyFormals -> T TL.Ty
translateCoercionArguments tyEnv formals = do
  let constrName = tupleName $ length (G.unTyFormals formals)
  translatedCoercionTypes <-
    mapM
      ( \(name, bound) -> do
          let tyvar = TL.TyVar $ TL.TyVarName $ G.unTyVarName name
          target <- translateType tyEnv (maybeType bound)
          pure $ TL.TyArrow tyvar target
      )
      (G.unTyFormals formals)
  pure $ TL.TyConstr constrName translatedCoercionTypes

translateTypeAbstractions :: G.TyFormals -> TL.Ty -> TL.Ty
translateTypeAbstractions formals inner = do
  translateTypeAbstractions' (reverse (G.unTyFormals formals)) inner
translateTypeAbstractions' :: [(G.TyVarName, Maybe G.Type)] -> TL.Ty -> TL.Ty
translateTypeAbstractions' [] inner = inner
translateTypeAbstractions' ((tyVarName, _) : other) inner =
  translateTypeAbstractions' other (TL.TyForall (TL.TyVarName $ G.unTyVarName tyVarName) inner)

translateTypeAbstractionExpressions :: G.TyFormals -> TL.Exp -> TL.Exp
translateTypeAbstractionExpressions formals inner = do
  translateTypeAbstractionExpressions' (reverse (G.unTyFormals formals)) inner
translateTypeAbstractionExpressions' :: [(G.TyVarName, Maybe G.Type)] -> TL.Exp -> TL.Exp
translateTypeAbstractionExpressions' [] inner = inner
translateTypeAbstractionExpressions' ((tyVarName, _) : other) inner =
  translateTypeAbstractionExpressions' other (TL.ExpTyAbs (TL.TyVarName $ G.unTyVarName tyVarName) inner)

funArgs :: T.Text
funArgs = T.pack "$funArgs"
coercionArgs :: T.Text
coercionArgs = T.pack "$coercionArgs"
receiverCoercionArgs :: T.Text
receiverCoercionArgs = T.pack "$receiverCoercionArgs"
buildAbstractionExpression :: TyEnv -> TL.Exp -> Maybe (G.VarName, G.TyName, G.TyFormals) -> G.MeSpec -> T TL.Exp
buildAbstractionExpression oldTyEnv innerExpression receiver (G.MeSpec _meName (G.MeSig formals args _)) = do
  let tyEnv = joinTyEnvs oldTyEnv $ tyFormalsToTyEnv formals
  let funArgsVar = TL.VarName funArgs
  let coercionArgsVar = TL.VarName coercionArgs
  let receiverCoercionArgsVar = TL.VarName receiverCoercionArgs
  -- surround with case expression to be able to access the coercion args
  translatedCoercionArgs <- translateCoercionArguments tyEnv formals
  caseExpr <- case translatedCoercionArgs of
    TL.TyConstr name types -> do
      varPatterns <-
        mapM
          ( \(name, constraint) -> do
              let tyvar = TL.TyVar $ TL.TyVarName $ G.unTyVarName name
              constraintName <- case maybeType constraint of
                G.TyNamed name _ -> pure name
                _ -> failT ("Constraint " ++ prettyS constraint ++ " is not a named type")
              target <- translateType tyEnv (maybeType constraint)
              let coercionType = TL.TyArrow tyvar target
              let translatedName = getCoercionAbsName name constraintName
              pure $ TL.PatVar translatedName coercionType
          )
          (G.unTyFormals formals)
      pure $ TL.ExpCase (TL.ExpVar coercionArgsVar) [TL.PatClause (TL.PatConstr name types varPatterns) innerExpression]
    _ -> failT "Internal error: coercion arguments are not a tuple"
  caseExpr <- case receiver of
    Just (_receiverName, _receiverType, receiverTypeArgs) -> do
      translatedReceiverCoercionArgs <- translateCoercionArguments tyEnv receiverTypeArgs
      case translatedReceiverCoercionArgs of
        TL.TyConstr name types -> do
          varPatterns <-
            mapM
              ( \(name, constraint) -> do
                  let tyvar = TL.TyVar $ TL.TyVarName $ G.unTyVarName name
                  constraintName <- case maybeType constraint of
                    G.TyNamed name _ -> pure name
                    _ -> failT ("Constraint " ++ prettyS constraint ++ " is not a named type")
                  target <- translateType tyEnv (maybeType constraint)
                  let coercionType = TL.TyArrow tyvar target
                  let translatedName = getCoercionAbsName name constraintName
                  pure $ TL.PatVar translatedName coercionType
              )
              (G.unTyFormals receiverTypeArgs)
          pure $ TL.ExpCase (TL.ExpVar receiverCoercionArgsVar) [TL.PatClause (TL.PatConstr name types varPatterns) caseExpr]
        _ -> failT "Internal error: function arguments are not a tuple"
    Nothing -> pure caseExpr
  -- surround with case expression to be able to access the args
  translatedArgs <- translateFunctionArguments tyEnv args
  caseExpr <- case translatedArgs of
    TL.TyConstr name types -> do
      varPatterns <-
        mapM
          ( \(name, goType) -> do
              translatedType <- translateType tyEnv goType
              let translatedName = TL.VarName $ G.unVarName name
              pure $ TL.PatVar translatedName translatedType
          )
          args
      pure $ TL.ExpCase (TL.ExpVar funArgsVar) [TL.PatClause (TL.PatConstr name types varPatterns) caseExpr]
    _ -> failT "Internal error: function arguments are not a tuple"
  abstracted <- pure $ TL.ExpAbs funArgsVar translatedArgs caseExpr
  abstracted <- pure $ TL.ExpAbs (TL.VarName coercionArgs) translatedCoercionArgs abstracted
  abstracted <- pure $ translateTypeAbstractionExpressions formals abstracted
  abstracted <- case receiver of
    Just (receiverName, receiverType, receiverTypeArgs) -> do
      let tyVars = map (\(tyVar, _) -> G.TyVar tyVar) (G.unTyFormals receiverTypeArgs)
      translatedReceiverType <- translateType tyEnv (G.TyNamed receiverType tyVars)
      abstracted <- pure $ TL.ExpAbs (TL.VarName $ G.unVarName receiverName) translatedReceiverType abstracted
      translatedReceiverCoercionArgs <- translateCoercionArguments tyEnv receiverTypeArgs
      abstracted <- pure $ TL.ExpAbs (TL.VarName receiverCoercionArgs) translatedReceiverCoercionArgs abstracted
      pure $ translateTypeAbstractionExpressions receiverTypeArgs abstracted
    Nothing -> pure abstracted
  pure abstracted

methods :: TyEnv -> G.Type -> T [G.MeSpec]
methods tyEnv ty = withCtx ("computing methods for " ++ prettyS ty) $ do
  classified <- classifyTy ty
  case classified of
    TyKindTyVar a -> do
      sigma <- getBound tyEnv a
      methods tyEnv sigma
    TyKindBuiltin _ ->
      pure []
    TyKindStruct tyName _ -> do
      decls <- allMethodsForStructOrBuiltin tyName
      pure $ map (\x -> me_spec x) decls
    TyKindIface t args -> do
      iface <- lookupIface t
      subst <- inst (if_formals iface) args
      pure $ G.applyTySubst subst (if_methods iface)

-- taken from existing type directed translation
inst :: G.TyFormals -> [G.Type] -> T G.TySubst
inst (G.TyFormals formals) types = do
  when (length formals /= length types) $
    failT ("Cannot instantiate " ++ prettyS formals ++ " with " ++ prettyS types ++ ": mismatch in number of arguments")
  pure $ Map.fromList (zipWith (\(a, _) replacement -> (a, replacement)) formals types)

isSubtype :: TyEnv -> G.Type -> G.Type -> T Bool
isSubtype tyEnv supertype possibleSubtype = do
  classifiedSupertype <- classifyTy supertype
  classifiedSubtype <- classifyTy possibleSubtype
  case (classifiedSupertype, classifiedSubtype) of
    (TyKindTyVar _, TyKindTyVar _) -> pure $ possibleSubtype == supertype
    (TyKindStruct _ _, TyKindStruct _ _) -> pure $ possibleSubtype == supertype
    (TyKindBuiltin _, TyKindBuiltin _) -> pure $ possibleSubtype == supertype
    (TyKindIface _ _, _) -> do
      methodsSuper <- methods tyEnv supertype
      methodsSub <- methods tyEnv possibleSubtype
      trace $ T.pack $ show methodsSuper
      trace $ T.pack $ show methodsSub
      pure $ Set.fromList methodsSuper `Set.isSubsetOf` Set.fromList methodsSub
    _ -> pure False

generateCoercion :: TyEnv -> (G.Type, TL.Exp) -> G.Type -> T TL.Exp
generateCoercion tyEnv (originalType, originalExp) targetType = do
  (_, coercionAbs) <- generateCoercionAbs tyEnv originalType targetType
  pure $ TL.ExpApp coercionAbs originalExp

generateCoercionAbs :: TyEnv -> G.Type -> G.Type -> T (TL.Ty, TL.Exp)
generateCoercionAbs tyEnv originalType targetType =
  if originalType == targetType
    then do
      let originalVarName = TL.VarName "original"
      let originalExp = TL.ExpVar originalVarName
      translatedOrigType <- translateType tyEnv originalType
      translatedTargetType <- translateType tyEnv targetType
      let coercionType = TL.TyArrow translatedOrigType translatedTargetType
      pure (coercionType, TL.ExpAbs originalVarName translatedOrigType originalExp)
    else generateCoercionAbs' tyEnv originalType targetType
generateCoercionAbs' :: TyEnv -> G.Type -> G.Type -> T (TL.Ty, TL.Exp)
generateCoercionAbs' tyEnv originalType (G.TyVar targetTyVar) = do
  ty <- lookupTyVar targetTyVar tyEnv
  generateCoercionAbs' tyEnv originalType ty
generateCoercionAbs' tyEnv originalType targetType = do
  let originalVarName = TL.VarName "original"
  let originalExp = TL.ExpVar originalVarName
  (originalTypeSubstituted, originalExpSubstituted) <- applyCoercion tyEnv (originalType, originalExp)
  translatedOrigType <- translateType tyEnv originalType
  translatedTargetType <- translateType tyEnv targetType
  let coercionType = TL.TyArrow translatedOrigType translatedTargetType
  classifiedTargetType <- classifyTy targetType
  case classifiedTargetType of
    TyKindIface ifaceName typeArgs -> do
      iface <- lookupIface ifaceName
      translatedTypeArgs <- mapM (translateType tyEnv) typeArgs
      let methodSpecs = if_methods iface
      coercions <-
        mapM
          ( \(G.MeSpec meName _signature) -> do
              emptyVarEnv <- mkVarEnv []
              (_, coerced) <-
                methodCallOnType emptyVarEnv tyEnv meName originalExpSubstituted Nothing originalTypeSubstituted Nothing
              pure coerced
          )
          methodSpecs
      let constr = TL.ExpConstr (TL.ConstrName $ interfacePrefix <> G.unTyName ifaceName) translatedTypeArgs coercions
      pure (coercionType, TL.ExpAbs originalVarName translatedOrigType constr)
    _ -> failT ("Can only coerce to an interface, not to " ++ prettyS targetType)

translateMethodBody :: VarEnv -> TyEnv -> G.MeBody -> T (G.Type, TL.Exp)
translateMethodBody varEnv tyEnv methodBody =
  translateBindings varEnv tyEnv (G.mb_bindings methodBody) (G.mb_return methodBody)

translateVarName :: G.VarName -> TL.VarName
translateVarName (G.VarName name) = TL.VarName name

-- Maybe I'll find a better function name in the future
translateBindings :: VarEnv -> TyEnv -> [(G.VarName, Maybe G.Type, G.Exp)] -> Maybe G.Exp -> T (G.Type, TL.Exp)
translateBindings varEnv tyEnv [] mainExp = translateMethodBodyReturn varEnv tyEnv mainExp
translateBindings varEnv tyEnv ((name, ty, exp) : otherBindings) mainExp = withCtx ("translation of binding " ++ prettyS name) $ do
  let translatedName = translateVarName name
  (expressionType, translatedExpression) <- withCtx "translation of expression to bind" $ translateExpression varEnv tyEnv exp
  bindingType <- case ty of
    Just x -> pure x
    Nothing -> pure expressionType
  coercedExpression <-
    if bindingType /= expressionType
      then withCtx "coercion to binding type" $ generateCoercion tyEnv (expressionType, translatedExpression) bindingType
      else pure translatedExpression
  translatedType <- translateType tyEnv bindingType
  updatedVarEnv <- extendVarEnv varEnv [(name, bindingType)]
  (innerExpressionType, innerExpression) <- translateBindings updatedVarEnv tyEnv otherBindings mainExp
  pure (innerExpressionType, TL.ExpApp (TL.ExpAbs translatedName translatedType innerExpression) coercedExpression)

translateMethodBodyReturn :: VarEnv -> TyEnv -> Maybe G.Exp -> T (G.Type, TL.Exp)
translateMethodBodyReturn _ _ Nothing = pure (G.tyVoid, TL.ExpVoid)
translateMethodBodyReturn varEnv tyEnv (Just mainExp) = translateExpression varEnv tyEnv mainExp

extractExpectedReceiverType :: MeDecl -> G.Type
extractExpectedReceiverType (MeDecl _ tyName formals _ _) =
  G.TyNamed tyName (map (\x -> maybeType (snd x)) $ G.unTyFormals formals)

coerceArguments :: TyEnv -> [(G.Type, TL.Exp)] -> [G.Type] -> T [(G.Type, TL.Exp)]
coerceArguments tyEnv sources targets = do
  let combined = zip sources targets
  coercedExpressionsAndTypes <-
    mapM (\(source, targetType) -> coerceArgument tyEnv source targetType) combined
  pure coercedExpressionsAndTypes

coerceArgument :: TyEnv -> (G.Type, TL.Exp) -> G.Type -> T (G.Type, TL.Exp)
coerceArgument tyEnv source targetType = do
  classifiedSource <- classifyTy (fst source)
  classifiedTarget <- classifyTy targetType
  case (classifiedTarget, classifiedSource) of
    (TyKindStruct targetName _constraints, TyKindStruct sourceName _tyArgs) -> do
      let sameStruct = targetName == sourceName
      unless sameStruct $ failT $ "Cannot supply " ++ prettyS sourceName ++ " when " ++ prettyS targetName ++ " is expected"
      pure source
    (TyKindTyVar _, _) -> pure source -- If this dosn't work, the translation should already fail on the coercion arguments
    (TyKindIface _ _, TyKindTyVar tyVarName) -> do
      resolved <- lookupTyVar tyVarName tyEnv
      tyName <- case resolved of
        G.TyNamed name _ -> pure name
        _ -> failT ("Type variable " ++ prettyS tyVarName ++ " did not resolve to a named type")
      let coercionAbs = getCoercionAbs tyVarName tyName
      let substitudedSource = (resolved, TL.ExpApp coercionAbs (snd source))
      coercedExp <- generateCoercion tyEnv substitudedSource targetType
      pure (targetType, coercedExp)
    _ -> do
      coercedExp <- generateCoercion tyEnv source targetType
      pure (targetType, coercedExp)

methodCall
  :: VarEnv
  -> TyEnv
  -> TL.Exp
  -> Maybe (TL.Exp, G.Type, [G.Type], G.Type)
  -> Maybe [G.Exp]
  -> [G.Type]
  -> Maybe [G.Type]
  -> [G.Type]
  -> [G.Type]
  -> G.TyFormals
  -> T TL.Exp
methodCall varEnv tyEnv methodVar receiver args expectedArgTypes methodTypeArgs receiverConstraints methodConstraints methodFormals = withCtx "methodCall" $ do
  receiverApplied <- case receiver of
    Just (translatedReceiverExpression, receiverType, receiverTypeArgs, expectedReceiverType) -> withCtx "receiverArgs" $ do
      -- apply type arguments from receiver
      translatedReceiverTypeArgs <- mapM (translateType tyEnv) receiverTypeArgs
      let typesApplied = generateTypeApplication methodVar translatedReceiverTypeArgs
      -- apply receiver coercion parameters
      generatedRecvCoercions <-
        mapM (\(originalType, targetType) -> generateCoercionAbs tyEnv originalType targetType) (zip receiverTypeArgs receiverConstraints)
      (recvCoercionTypes, generatedRecvCoercions) <- pure $ unzip generatedRecvCoercions
      let receiverCoercions = TL.ExpConstr (tupleName $ length receiverTypeArgs) recvCoercionTypes generatedRecvCoercions
      let recvCoercionsApplied = TL.ExpApp typesApplied receiverCoercions
      -- apply receiver
      (_, coercedReceiver) <- coerceArgument tyEnv (receiverType, translatedReceiverExpression) expectedReceiverType
      pure $ TL.ExpApp recvCoercionsApplied coercedReceiver
    Nothing -> pure methodVar
  (methodCoercionsApplied, substitutions) <- case methodTypeArgs of
    Just methodTypeArgs -> withCtx "methodCoercionArgs" $ do
      translatedMethodTypeArgs <- mapM (translateType tyEnv) methodTypeArgs
      let methodTypesApplied = generateTypeApplication receiverApplied translatedMethodTypeArgs
      let substitutions = zip (map fst (G.unTyFormals methodFormals)) methodTypeArgs
      let substitutedMethodConstraints = map (substituteTypeVariables substitutions) methodConstraints
      generatedCoercions <-
        mapM (\(originalType, targetType) -> generateCoercionAbs tyEnv originalType targetType) (zip methodTypeArgs substitutedMethodConstraints)
      (coercionTypes, generatedCoercions) <- pure $ unzip generatedCoercions
      let methodCoercions = TL.ExpConstr (tupleName $ length generatedCoercions) coercionTypes generatedCoercions
      pure (TL.ExpApp methodTypesApplied methodCoercions, substitutions)
    Nothing -> pure (receiverApplied, [])
  argsApplied <- case args of
    Just args -> withCtx "funArgs" $ do
      let substitutedExpectedArgTypes = map (substituteTypeVariables substitutions) expectedArgTypes
      translatedArgs <- mapM (translateExpression varEnv tyEnv) args
      translatedArgs <- coerceArguments tyEnv translatedArgs substitutedExpectedArgTypes
      let argTypes = map fst translatedArgs
      translatedArgTypes <- mapM (translateType tyEnv) argTypes
      pure $ TL.ExpApp methodCoercionsApplied (TL.ExpConstr (tupleName $ length translatedArgs) translatedArgTypes (map snd translatedArgs))
    Nothing -> pure methodCoercionsApplied
  pure argsApplied

methodCallOnType
  :: VarEnv
  -> TyEnv
  -> G.MeName
  -> TL.Exp
  -> Maybe [G.Exp]
  -> G.Type
  -> Maybe [G.Type]
  -> T (G.Type, TL.Exp)
methodCallOnType varEnv tyEnv meName translatedReceiverExpression args receiverType methodTypeArgs = withCtx ("methodCallOnType for " ++ (T.unpack $ G.unMeName meName)) $ do
  classified <- classifyTy receiverType
  case classified of
    TyKindStruct typeName typeArgs -> do
      struct <- lookupStruct typeName
      methodDecls <- allMethodsForStructOrBuiltin typeName
      let maybeDecl = List.find (\x -> (G.ms_name $ me_spec x) == meName) methodDecls
      decl <- case maybeDecl of
        Just x -> pure x
        Nothing -> failT ("Method declaration for struct not found: " ++ prettyS meName)
      let spec = me_spec decl
      let resultType = G.msig_res $ G.ms_sig spec
      let methodTypeArgsUnpacked = fromMaybe [] methodTypeArgs
      substitutions <- pure $ zip (map fst (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)) methodTypeArgsUnpacked
      -- Add type args from receiver to substitutions
      substitutions <- pure $ substitutions ++ zip (map fst (G.unTyFormals $ st_formals struct)) typeArgs
      let substitutedResultType = substituteTypeVariables substitutions resultType
      substMeTyArgs <- case methodTypeArgs of
        Just meTyArgs -> inst (G.msig_tyArgs $ G.ms_sig spec) meTyArgs
        Nothing -> pure Map.empty
      substRecvTyArgs <- inst (st_formals struct) typeArgs
      expectedArgTypes <- pure $ map snd $ G.msig_args $ G.ms_sig spec
      expectedArgTypes <- pure $ G.applyTySubst (Map.union substMeTyArgs substRecvTyArgs) expectedArgTypes
      let methodVar = TL.ExpVar $ TL.VarName $ G.unMeName meName <> "_" <> G.unTyName typeName
      substRecv <- inst (me_formals decl) typeArgs
      receiverConstraints <- pure $ map (\x -> maybeType (snd x)) (G.unTyFormals $ me_formals decl)
      receiverConstraints <- pure $ G.applyTySubst substRecv receiverConstraints
      expectedReceiverType <- pure $ extractExpectedReceiverType decl
      expectedReceiverType <- pure $ G.applyTySubst substRecv expectedReceiverType
      constraints <- pure $ map (\x -> maybeType (snd x)) (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)
      constraints <- case methodTypeArgs of
        Just meTyArgs -> do
          subst <- inst (G.msig_tyArgs $ G.ms_sig spec) meTyArgs
          pure $ G.applyTySubst subst constraints
        Nothing -> pure constraints
      argsApplied <-
        methodCall
          varEnv
          tyEnv
          methodVar
          (Just (translatedReceiverExpression, receiverType, typeArgs, expectedReceiverType))
          args
          expectedArgTypes
          methodTypeArgs
          receiverConstraints
          constraints
          (G.msig_tyArgs $ G.ms_sig spec)
      pure (substitutedResultType, argsApplied)
    TyKindIface typeName typeArgs -> do
      iface <- lookupIface typeName
      let formals = if_formals iface
      translatedTypeArgs <- mapM (translateType tyEnv) typeArgs
      methodSpecs <- pure $ if_methods iface
      substRecvTyArgs <- inst formals typeArgs
      methodSpecs <- pure $ map (\spec -> G.applyTySubst substRecvTyArgs spec) methodSpecs
      let maybeSpec = List.find (\x -> G.ms_name x == meName) methodSpecs
      spec <- case maybeSpec of
        Just x -> pure x
        Nothing -> failT ("Method for interface not found: " ++ show meName)
      subst <- case methodTypeArgs of
        Just meTyArgs -> inst (G.msig_tyArgs $ G.ms_sig spec) meTyArgs
        Nothing -> pure Map.empty
      spec <- pure $ G.applyTySubst subst spec
      let resultType = G.msig_res $ G.ms_sig spec
      let methodTypeArgsUnpacked = fromMaybe [] methodTypeArgs
      let substitutions = zip (map fst (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)) methodTypeArgsUnpacked
      let substitutedResultType = substituteTypeVariables substitutions resultType
      let expectedArgTypes = map snd $ G.msig_args $ G.ms_sig spec
      translatedMethodSpecs <- mapM (translateMethodSpec tyEnv Nothing) methodSpecs
      let ifaceMethodNames = map (\(G.MeSpec name _) -> name) methodSpecs
      let ifaceTypesWithName = zip ifaceMethodNames translatedMethodSpecs
      let patterns = map (generateSelectPattern2 meName) ifaceTypesWithName
      let methodVar = TL.ExpVar selectVarName
      let receiverConstraints = [] -- these contraints are not of any use
      let constraints = map (\x -> maybeType (snd x)) (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)
      argsApplied <-
        methodCall
          varEnv
          tyEnv
          methodVar
          Nothing
          args
          expectedArgTypes
          methodTypeArgs
          receiverConstraints
          constraints
          (G.msig_tyArgs $ G.ms_sig spec)
      let outerCase =
            TL.ExpCase
              translatedReceiverExpression
              [TL.PatClause (TL.PatConstr (TL.ConstrName $ interfacePrefix <> G.unTyName typeName) translatedTypeArgs patterns) argsApplied]
      pure (substitutedResultType, outerCase)
    TyKindTyVar t -> do
      resolved <- lookupTyVar t tyEnv
      methodCallOnType varEnv tyEnv meName translatedReceiverExpression args resolved methodTypeArgs
    TyKindBuiltin builtinType -> do
      -- TODO this branch is almost a duplicate of TyKindStruct
      methodDecls <- allMethodsForStructOrBuiltin (tyBuiltinToTyName builtinType)
      let maybeDecl = List.find (\x -> (G.ms_name $ me_spec x) == meName) methodDecls
      decl <- case maybeDecl of
        Just x -> pure x
        Nothing ->
          failT ("Method declaration for builtin" ++ prettyS (tyBuiltinToTyName builtinType) ++ " not found: " ++ prettyS meName)
      let spec = me_spec decl
      let resultType = G.msig_res $ G.ms_sig spec
      let methodTypeArgsUnpacked = fromMaybe [] methodTypeArgs
      let substitutions = zip (map fst (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)) methodTypeArgsUnpacked
      let substitutedResultType = substituteTypeVariables substitutions resultType
      expectedArgTypes <- pure $ map snd $ G.msig_args $ G.ms_sig spec
      expectedArgTypes <- case methodTypeArgs of
        Just meTyArgs -> do
          subst <- inst (G.msig_tyArgs $ G.ms_sig spec) meTyArgs
          pure $ G.applyTySubst subst expectedArgTypes
        Nothing -> pure expectedArgTypes
      let methodVar = TL.ExpVar $ TL.VarName $ G.unMeName meName <> "_" <> G.unTyName (tyBuiltinToTyName builtinType)
      let receiverConstraints = []
      let expectedReceiverType = extractExpectedReceiverType decl
      constraints <- pure $ map (\x -> maybeType (snd x)) (G.unTyFormals $ G.msig_tyArgs $ G.ms_sig spec)
      constraints <- case methodTypeArgs of
        Just meTyArgs -> do
          subst <- inst (G.msig_tyArgs $ G.ms_sig spec) meTyArgs
          pure $ G.applyTySubst subst constraints
        Nothing -> pure constraints
      argsApplied <-
        methodCall
          varEnv
          tyEnv
          methodVar
          (Just (translatedReceiverExpression, receiverType, [], expectedReceiverType))
          args
          expectedArgTypes
          methodTypeArgs
          receiverConstraints
          constraints
          (G.msig_tyArgs $ G.ms_sig spec)
      pure (substitutedResultType, argsApplied)

methodCallFun :: VarEnv -> TyEnv -> G.MeName -> [G.Exp] -> [G.Type] -> T (G.Type, TL.Exp)
methodCallFun varEnv tyEnv meName args methodTypeArgs = do
  signature <- lookupFun meName
  expectedArgTypes <- pure $ map snd (G.msig_args signature)
  subst <- inst (G.msig_tyArgs signature) methodTypeArgs
  expectedArgTypes <- pure $ G.applyTySubst subst expectedArgTypes
  let methodVar = TL.ExpVar $ TL.VarName $ G.unMeName meName
  let constraints = map (\x -> maybeType (snd x)) $ G.unTyFormals $ G.msig_tyArgs signature
  translatedExp <-
    methodCall varEnv tyEnv methodVar Nothing (Just args) expectedArgTypes (Just methodTypeArgs) [] constraints (G.msig_tyArgs signature)
  pure (G.msig_res signature, translatedExp)

typeOfField :: Struct -> [G.Type] -> G.FieldName -> T G.Type
typeOfField struct tyArgs fieldName = do
  let substitutions = zip (map fst (G.unTyFormals $ st_formals struct)) tyArgs
  case List.find (\x -> fieldName == fst x) (st_fields struct) of
    Just (_, t) -> pure $ substituteTypeVariables substitutions t
    Nothing -> pure G.tyVoid -- Should not happen...?

substituteTypeVariables :: [(G.TyVarName, G.Type)] -> G.Type -> G.Type
substituteTypeVariables substitutions (G.TyVar var) =
  let maybeSubst = List.find (\(tyVarName, _) -> tyVarName == var) substitutions
   in case maybeSubst of
        Just (_, ty) -> ty
        Nothing -> G.TyVar var
substituteTypeVariables substitutions (G.TyNamed name typeArgs) = G.TyNamed name (map (substituteTypeVariables substitutions) typeArgs)

selectVarName :: TL.VarName
selectVarName = TL.VarName $ T.pack "latestSelection"
generateSelectPattern :: G.FieldName -> (TL.Ty, (G.FieldName, G.Type)) -> TL.Pat
generateSelectPattern selectedFieldName (translatedType, (name, _)) =
  if name == selectedFieldName
    then TL.PatVar selectVarName translatedType
    else TL.PatWild translatedType
generateSelectPattern2 :: G.MeName -> (G.MeName, TL.Ty) -> TL.Pat
generateSelectPattern2 selectedName (currentName, currentType) = do
  if selectedName == currentName
    then TL.PatVar selectVarName currentType
    else TL.PatWild currentType

generateTypeApplication :: TL.Exp -> [TL.Ty] -> TL.Exp
generateTypeApplication left [] = left
generateTypeApplication tyAbs (arg : otherArgs) = generateTypeApplication (TL.ExpTyApp tyAbs arg) otherArgs

substituteTypeArgs :: G.Type -> [G.Type] -> T G.Type
substituteTypeArgs (G.TyNamed name _) newTypeArgs = pure $ G.TyNamed name newTypeArgs
substituteTypeArgs (G.TyVar name) _ = failT $ "Cannot substitute types for type variable " ++ prettyS name

getCoercionAbsName :: G.TyVarName -> G.TyName -> TL.VarName
getCoercionAbsName tyVarName namedTargetType = TL.VarName $ G.unTyVarName tyVarName <> "To" <> G.unTyName namedTargetType

getCoercionAbs :: G.TyVarName -> G.TyName -> TL.Exp
getCoercionAbs tyVarName namedTargetType = TL.ExpVar $ getCoercionAbsName tyVarName namedTargetType

applyCoercion :: TyEnv -> (G.Type, TL.Exp) -> T (G.Type, TL.Exp)
applyCoercion tyEnv (ty, exp) = do
  case ty of
    G.TyVar name -> withCtx ("insertion of coercion application for " ++ prettyS exp) $ do
      resolved <- lookupTyVar name tyEnv
      tyName <- case resolved of
        G.TyNamed name _ -> pure name
        _ -> failT ("Type variable " ++ prettyS name ++ " did not resolve to a named type")
      let coercionAbs = getCoercionAbs name tyName
      pure (resolved, TL.ExpApp coercionAbs exp)
    _ -> pure (ty, exp)

applyCoercionTo :: TyEnv -> (G.Type, TL.Exp) -> G.Type -> T (G.Type, TL.Exp)
applyCoercionTo tyEnv (ty, exp) resultType =
  if ty == resultType
    then pure (ty, exp)
    else do
      case ty of
        G.TyVar name -> withCtx ("insertion of coercion application to " ++ prettyS resultType ++ " for " ++ prettyS exp) $ do
          tyName <- case resultType of
            G.TyNamed name _ -> pure name
            _ -> failT ("Type variable " ++ prettyS name ++ " did not resolve to a named type")
          let coercionAbs = getCoercionAbs name tyName
          pure (resultType, TL.ExpApp coercionAbs exp)
        G.TyNamed _ _ -> do
          coerced <- generateCoercion tyEnv (ty, exp) resultType
          pure (resultType, coerced)

-- todo the usage of this function should probably be replaced with translateExpressionAndCoerce
translateExpressionAndSub :: VarEnv -> TyEnv -> G.Exp -> T (G.Type, TL.Exp)
translateExpressionAndSub varEnv tyEnv exp = do
  (resultType, translatedExpression) <- translateExpression varEnv tyEnv exp
  applyCoercion tyEnv (resultType, translatedExpression)

translateExpressionAndCoerce :: VarEnv -> TyEnv -> (G.Exp, G.Type) -> T (G.Type, TL.Exp)
translateExpressionAndCoerce varEnv tyEnv (exp, targetType) = do
  (resultType, translatedExpression) <- translateExpression varEnv tyEnv exp
  applyCoercionTo tyEnv (resultType, translatedExpression) targetType

translateExpression :: VarEnv -> TyEnv -> G.Exp -> T (G.Type, TL.Exp)
translateExpression _ _ (G.BoolLit value) = pure (tyBuiltinToType TyBool, TL.ExpBool value)
translateExpression _ _ (G.IntLit value) = pure (tyBuiltinToType TyInt, TL.ExpInt value)
translateExpression _ _ (G.StrLit value) = pure (tyBuiltinToType TyString, TL.ExpStr value)
translateExpression varEnv _ (G.Var varName) = do
  goType <- lookupVar varName varEnv
  pure (goType, TL.ExpVar (translateVarName varName))
translateExpression varEnv tyEnv (G.StructLit structType fieldValues) = do
  assertTypeOk tyEnv structType
  actualType <- classifyTy structType
  case actualType of
    TyKindStruct typeName _ -> do
      goStruct <- lookupStruct typeName
      typeArgs <- case structType of
        G.TyNamed _ tys -> pure tys
        _ -> failT ("Struct literal requires a named type, not " ++ prettyS structType)
      translatedTypeArgs <- mapM (translateType tyEnv) typeArgs
      substitutedStructType <- substituteTypeArgs structType typeArgs
      let fields = st_fields goStruct
      when (length fieldValues /= length fields) $
        failT ("Invalid number of arguments for construction of struct " ++ prettyS substitutedStructType)
      let substitutions = zip (map fst (G.unTyFormals $ st_formals goStruct)) typeArgs
      let substitutedFields = map (substituteTypeVariables substitutions . snd) fields
      translatedFieldValues <- mapM (translateExpressionAndCoerce varEnv tyEnv) (zip fieldValues substitutedFields)
      pure (substitutedStructType, TL.ExpConstr (TL.ConstrName $ structPrefix <> G.unTyName typeName) translatedTypeArgs (map snd translatedFieldValues))
    _ -> failT ("Invalid type for constructing a struct: " ++ prettyS structType)
translateExpression varEnv tyEnv (G.Select exp fieldName) = do
  (expType, translatedExpression) <- withCtx "translation of expression to select on" $ translateExpression varEnv tyEnv exp
  classified <- classifyTy expType
  case classified of
    TyKindStruct name tyArgs -> do
      goStruct <- lookupStruct name
      let substitutions = zip (map fst (G.unTyFormals $ st_formals goStruct)) tyArgs
      let fields = map snd $ st_fields goStruct
      let substitutedFields = map (substituteTypeVariables substitutions) fields
      translatedFieldTypes <- mapM (translateType tyEnv) substitutedFields
      let patterns = zipWith (curry (generateSelectPattern fieldName)) translatedFieldTypes (st_fields goStruct)
      translatedTyArgs <- mapM (translateType tyEnv) tyArgs
      resultType <- typeOfField goStruct tyArgs fieldName
      let pat =
            TL.PatClause
              (TL.PatConstr (TL.ConstrName $ structPrefix <> G.unTyName name) translatedTyArgs patterns)
              (TL.ExpVar selectVarName)
      pure (resultType, TL.ExpCase translatedExpression [pat])
    _ -> failT ("Field selection requires a struct, but " ++ show exp ++ " is not a struct")
translateExpression varEnv tyEnv (G.BinOp binOp left right) = do
  (tl, translatedLeft) <- translateExpression varEnv tyEnv left
  (tr, translatedRight) <- translateExpression varEnv tyEnv right
  ty <- case Map.lookup binOp binOpTypes of
    Just m -> case Map.lookup (tl, tr) m of
      Just ty -> pure ty
      Nothing -> failT ("Binary operator " ++ show binOp ++ " does not support the types " ++ show tl ++ " and " ++ show tr)
    Nothing -> failT ("Unknown binary operator: " ++ show binOp)
  pure (ty, TL.ExpBinOp translatedLeft binOp translatedRight)
translateExpression varEnv tyEnv (G.UnOp op expression) = do
  (resultType, translatedExpression) <- translateExpression varEnv tyEnv expression
  case op of
    Not -> when (resultType /= tyBuiltinToType TyBool) $ failT ("Invalid type for not: " ++ prettyS resultType)
    Inv -> when (resultType /= tyBuiltinToType TyInt) $ failT ("Invalid type for -: " ++ prettyS resultType)
  pure (resultType, TL.ExpUnOp op translatedExpression)
translateExpression varEnv tyEnv (G.MeCall (G.Var (G.VarName "fmt")) meName _ args) = do
  translatedArgsWithTypes <- mapM (translateExpressionAndSub varEnv tyEnv) args
  let translatedArgs = map snd translatedArgsWithTypes
  let rawMeName = T.unpack $ G.unMeName meName
  _ <- pure $ unless (rawMeName `elem` ["Printf", "Sprintf"]) (failT $ "Method is not part of fmt: " ++ show meName)
  (formatStringType, formatString) <- pure $ head translatedArgsWithTypes
  when (formatStringType /= tyBuiltinToType TyString) $
    failT ("Printf/Sprintf requires a string literal as the first argument, not " ++ show formatStringType)
  let formatStringLit =
        case formatString of
          TL.ExpStr fmtStr -> fmtStr
          _ -> "(s)printf only supports string literals as format strings"
  let valueArgs = tail translatedArgs
  if rawMeName == "Printf"
    then pure (G.tyVoid, TL.ExpPrintf formatStringLit valueArgs)
    else pure (tyBuiltinToType TyString, TL.ExpSprintf formatStringLit valueArgs)
translateExpression varEnv tyEnv (G.MeCall exp meName typeArgs args) = do
  (receiverType, translatedReceiverExpression) <- translateExpressionAndSub varEnv tyEnv exp
  methodCallOnType varEnv tyEnv meName translatedReceiverExpression (Just args) receiverType (Just typeArgs)
translateExpression varEnv tyEnv (G.FunCall meName typeArgs args) = do
  methodCallFun varEnv tyEnv meName args typeArgs
translateExpression varEnv tyEnv (G.Cond a b c) = do
  (typeA, translatedA) <- translateExpression varEnv tyEnv a
  (typeB, translatedB) <- translateExpression varEnv tyEnv b
  (typeC, translatedC) <- translateExpression varEnv tyEnv c
  when (typeA /= tyBuiltinToType TyBool) $ failT ("Condition is not of type bool: " ++ show a)
  bSubtypeOfC <- isSubtype tyEnv typeC typeB
  cSubtypeOfB <- isSubtype tyEnv typeB typeC
  if typeB == typeC
    then pure (typeB, TL.ExpCond translatedA translatedB translatedC)
    else
      if cSubtypeOfB
        then do
          coercedC <- generateCoercion tyEnv (typeC, translatedC) typeB
          pure (typeB, TL.ExpCond translatedA translatedB coercedC)
        else
          if bSubtypeOfC
            then do
              coercedB <- generateCoercion tyEnv (typeB, translatedB) typeC
              pure (typeC, TL.ExpCond translatedA coercedB translatedC)
            else failT ("Incompatible types in branches: " ++ show b ++ " and " ++ show c)
translateExpression _ _ e = failT ("I don't know how to translate this expression yet: " ++ show e)

generateMainDecls :: [G.MeBody] -> Int -> [G.Decl]
generateMainDecls [] _ = []
generateMainDecls (body : other) idx =
  let suffix =
        if idx == 1
          then ""
          else show idx
      spec = G.MeSpec (G.MeName $ T.pack $ "main" ++ suffix) (G.MeSig (G.TyFormals []) [] G.tyVoid)
   in G.FunDecl spec body : generateMainDecls other (idx + 1)
pseudoMain :: Int -> TL.Exp
pseudoMain idx =
  let suffix =
        if idx == 1
          then ""
          else show idx
      emptyArgs = TL.ExpConstr (tupleName 0) [] []
      inner = TL.ExpApp (TL.ExpVar $ TL.VarName $ T.pack $ "main" ++ suffix) emptyArgs
   in TL.ExpApp inner emptyArgs

translateProgram :: G.Program -> T TL.Prog
translateProgram (G.Program _ []) = failT "No main method found"
translateProgram program = do
  translatedDeclarationsNested <-
    mapM translateDeclaration (G.p_decls program ++ generateMainDecls (G.p_mains program) 1)
  translatedDeclarations <- pure $ concat translatedDeclarationsNested
  translatedDeclarations <- pure $ stdLib ++ translatedDeclarations
  let translatedMain = pseudoMain $ length $ G.p_mains program
  trace $ T.pack $ prettyS (TL.Prog translatedDeclarations translatedMain)
  pure $ TL.Prog translatedDeclarations translatedMain

stdLib :: [TL.Decl]
stdLib = generateGenericTuples 20
