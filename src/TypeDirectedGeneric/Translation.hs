{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TypeDirectedGeneric.Translation (

  runTranslation, runTranslation', stdlibForTrans

) where

import Common.Utils
import Common.PrettyUtils
import Common.Types
import qualified Common.FGGAST as G
import Common.FGGPretty ()
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL
import Prettyprinter
import TypeDirectedGeneric.TransCommon

import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Generics
import qualified Data.Map.Strict as Map
import Control.Monad.Identity
import Control.Monad.Extra
import qualified Data.Text as T
import qualified Data.List as List
import Data.Maybe
import System.IO
import System.Exit
import Text.RawString.QQ

--
-- Types
--

type TyExpEnv = GenTyExpEnv TL.Exp

--
-- Auxilary functions
--

var :: TL.Var -> TL.Exp
var = TL.ExpVar

mkSigList :: [TL.Exp] -> TL.Exp
mkSigList es = TL.expApp (TL.ExpConstr "-sig") (TL.ExpList es)

mkDictList :: [TL.Exp] -> TL.Exp
mkDictList = TL.ExpList

matchDictList :: [TL.Pat] -> TL.Pat
matchDictList = TL.PatList

freshVar :: T TL.Var
freshVar = do
  x <- genFreshVar
  pure (TL.Var x)

freshVars :: Int -> T [TL.Var]
freshVars i = do
  xs <- genFreshVars i
  pure (map TL.Var xs)

freshTyVars :: Int -> T [G.TyVarName]
freshTyVars i = do
  xs <- genFreshVars i
  pure (map G.TyVarName xs)

--
-- Auxiliary judgments
--

-- methods_{\Delta}(\tau)
methods :: TyEnv -> G.Type -> T [G.MeSpec]
methods tenv tau = withCtx ("computing methods for " ++ prettyS tau) $ do
  k <- classifyTy tau
  case k of
    TyKindTyVar a -> do
      sigma <- getBound tenv a
      methods tenv sigma
    TyKindBuiltin _ ->
      pure []
    TyKindStruct _ _ -> do
      m <- methodsStructOrBuiltin tenv tau
      pure $ map fst (Map.elems m)
    TyKindIface t args -> do
      methodsIface t args
  where
    methodsIface t args = do
      iface <- lookupIface t
      subst <- inst (if_formals iface) args
      pure $ G.applyTySubst subst (if_methods iface)

-- <S, E> \in methods(\Delta, \tau_S)
methodsStructOrBuiltin :: TyEnv -> G.Type -> T (Map G.MeName (G.MeSpec, TL.Exp))
methodsStructOrBuiltin tenv tau = do
  k <- classifyTy tau
  case k of
    TyKindTyVar _ ->
      failT ("Type " ++ prettyS tau ++ " is not a struct type")
    TyKindIface _ _ ->
      failT ("Type " ++ prettyS tau ++ " is not a struct type")
    TyKindBuiltin t -> getMethods (tyBuiltinToTyName t) []
    TyKindStruct t args -> getMethods t args
  where
    getMethods t args = do
      ms <- allMethodsForStructOrBuiltin t
      l <- flip mapMaybeM ms $ \m -> do
             mSubst <- tryInst tenv (me_formals m) args
             case mSubst of
               Left err -> do
                 trace
                   ("Cannot instantiate declaration of " <> prettyT (me_spec m) <> " for " <>
                    prettyT t <> " for arguments " <> prettyT args <> ": " <> T.pack err)
                 pure Nothing
               Right (subst, e) ->
                 let spec = G.applyTySubst subst (me_spec m)
                 in pure $ Just (G.ms_name spec, (spec, e))
      pure (Map.fromList l)

--
-- Translation of types
--

typeOfTyvar :: TL.Exp -> TL.Exp
typeOfTyvar = TL.fstOfTriple

typeStarOfTyvar :: TL.Exp -> TL.Exp
typeStarOfTyvar = TL.sndOfTriple

boundFunOfTyvar :: TL.Exp -> TL.Exp
boundFunOfTyvar = TL.thdOfTriple

-- \Delta,\mathcal T |- \tau ~> E
transType' :: TyEnv -> TyExpEnv -> G.Type -> T TL.Exp
transType' te@(TyEnv tenv) ee@(GenTyExpEnv expEnv) tau =
    case tau of
      G.TyVar a ->
          case (Map.lookup a tenv, Map.lookup a expEnv) of
            (Just _, Nothing) -> pure (typeOfTyvar (var (tyVarVar a)))
            (Nothing, Just e) -> pure e
            (Just _, Just _) ->
                failT ("Type variable " ++ prettyS a ++ " bound in type env and type-exp env. " ++
                       "\nType env: " ++ show tenv ++
                       "\nType exp env: " ++ show expEnv)
            (Nothing, Nothing) ->
                failT ("Unbound type variable " ++ prettyS a)
      G.TyNamed t [] ->
        pure $ TL.ExpConstr (constrForTyName t)
      G.TyNamed t sigmas -> do
        e <- transTypes te ee sigmas
        pure $ TL.expApp (TL.ExpConstr (constrForTyName t)) e

-- \Delta,\mathcal T |- \overline{\tau} ~> E
transTypes :: TyEnv -> TyExpEnv -> [G.Type] -> T TL.Exp
transTypes tenv expEnv taus = do
  es <- mapM (transType' tenv expEnv) taus
  pure (TL.mkTuple es)

-- \Delta |- \tau ~> E
transType :: TyEnv -> G.Type -> T TL.Exp
transType tenv tau = transType' tenv emptyTyExpEnv tau

transTypeStar :: TyEnv -> G.Type -> T TL.Exp
transTypeStar tenv tau = transTypeStar' tenv emptyTyExpEnv tau

transTypeStar' :: TyEnv -> TyExpEnv -> G.Type -> T TL.Exp
transTypeStar' tenv tyExpEnv tau =
  withCtx ("transTypeStar' " ++ show tau ++
           ", tenv=" ++ show tenv ++ ", tyExpEnv=" ++ show tyExpEnv) $ do
  k <- classifyTy tau
  case k of
    TyKindBuiltin _ -> transType' tenv tyExpEnv tau
    TyKindStruct _ _ -> transType' tenv tyExpEnv tau
    TyKindTyVar _ -> transType' tenv tyExpEnv tau
    TyKindIface t args -> do
      iface <- lookupIface t
      eta <- inst (if_formals iface) args
      es <- forM (if_methods iface) $ \spec -> transMethodSpec tenv tyExpEnv (G.applyTySubst eta spec)
      pure (mkSigList es)

freshMethodSig :: G.MeSig -> T G.MeSig
freshMethodSig sig = do
  let origVars = map fst (G.unTyFormals (G.msig_tyArgs sig))
  fresh <- freshTyVars (length origVars)
  let tyvarMap = Map.fromList (zip origVars fresh)
  pure $ everywhere (mkT (rewriteTyVar tyvarMap)) sig
  where
    rewriteTyVar :: Map.Map G.TyVarName G.TyVarName -> G.TyVarName -> G.TyVarName
    rewriteTyVar m v =
      case Map.lookup v m of
        Just x -> x
        Nothing -> v

-- \Delta |-_{sig} M ~> E
transMethodSig :: TyEnv -> TyExpEnv -> G.MeSig -> T TL.Exp
transMethodSig tenv initExpEnv sigOrig = withCtx ("transMethodSig " ++ show sigOrig) $ do
  sig <- freshMethodSig sigOrig
  let G.TyFormals formals = G.msig_tyArgs sig
  expEnv <-
    extendTyExpEnv initExpEnv
      (zipWith (\(a, _) i -> (a, TL.ExpConstr (constrI i))) formals [1..])
  e <- transTypes tenv expEnv (map (maybeType . snd) formals)
  e' <- transTypes tenv expEnv (map snd (G.msig_args sig) ++ [G.msig_res sig])
  pure (TL.mkTuple [e, e'])

-- \Delta |-_{spec} S ~> E
transMethodSpec :: TyEnv -> TyExpEnv -> G.MeSpec -> T TL.Exp
transMethodSpec tenv tyExpEnv spec = withCtx ("transMethodSpec " ++ show spec) $ do
  e <- transMethodSig tenv tyExpEnv (G.ms_sig spec)
  pure (TL.expApp (TL.ExpConstr (constrForMeName (G.ms_name spec))) e)

--
-- Instantiation of types
--

-- \Delta |-subst \Phi |-> \phi ~> E
tryInst :: TyEnv -> G.TyFormals -> [G.Type] -> T (Either String (G.TySubst, TL.Exp))
tryInst tyEnv (G.TyFormals formals) sigmas
    | length formals /= length sigmas = pure (Left "arity mismatch")
    | otherwise = do
        let subst = Map.fromList (zipWith (\(a, _) sigma -> (a, sigma)) formals sigmas)
            taus = map (maybeType . snd) formals
        eisM <-
            catchT
                (flip mapM (zip sigmas taus) $ \(sigma, tau) ->
                 dictCons tyEnv sigma (G.applyTySubst subst tau))
        case eisM of
          Left e -> pure (Left ("bound mismatch: " ++ e))
          Right eis'' -> do
            eis <- mapM (transType tyEnv) sigmas
            eis' <- mapM (transTypeStar tyEnv) sigmas
            let triples = map (\(e, e', e'') -> TL.mkTriple e e' e'') (zip3 eis eis' eis'')
            pure (Right (subst, TL.mkTuple triples))

instMeSigOrFail ::
  (String -> String) -> TyEnv -> G.MeSig -> [G.Type] -> T ([G.Type], G.Type, TL.Exp)
instMeSigOrFail errMsg tyEnv sig actuals = do
  mSubst <- tryInst tyEnv (G.msig_tyArgs sig) actuals
  case mSubst of
    Left x -> failT (errMsg x)
    Right (subst, e) ->
      pure ( G.applyTySubst subst (map snd (G.msig_args sig))
           , G.applyTySubst subst (G.msig_res sig)
           , e)

inst :: G.TyFormals -> [G.Type] -> T G.TySubst
inst (G.TyFormals formals) sigmas
    | length formals /= length sigmas =
        failT $
          "Cannot instantiate " ++ prettyS formals ++ " with " ++ prettyS sigmas ++
          ": mismatch in number of arguments"
    | otherwise =
        pure $ Map.fromList (zipWith (\(a, _) sigma -> (a, sigma)) formals sigmas)

data MatchType
  = TypeEq
  | TypeNotEq
  | TypeUnifiable

matchTypes :: G.Type -> G.Type -> MatchType
matchTypes tau sigma =
  case (tau, sigma) of
    (G.TyNamed t1 args1, G.TyNamed t2 args2)
      | t1 == t2 -> matchLists args1 args2
      | otherwise -> TypeNotEq
    (G.TyVar _, _) -> TypeUnifiable
    (_, G.TyVar _)  -> TypeUnifiable
  where
    matchLists ts1 ts2
      | length ts1 /= length ts2 = TypeNotEq
      | otherwise =
          let res = map (\(x, y) -> matchTypes x y) (zip ts1 ts2)
          in foldl combine TypeEq res
    combine TypeEq TypeEq = TypeEq
    combine TypeNotEq _ = TypeNotEq
    combine _ TypeNotEq = TypeNotEq
    combine TypeUnifiable TypeEq = TypeUnifiable
    combine TypeEq TypeUnifiable = TypeUnifiable
    combine TypeUnifiable TypeUnifiable = TypeUnifiable

--
-- Dictionary construction
--

-- \Delta |-dictCons \tau <: \sigma ~> E
dictCons :: TyEnv -> G.Type -> G.Type -> T TL.Exp
dictCons tenv tau sigma =
  withCtx ("dictCons " ++ prettyS tau ++ " <: " ++ prettyS sigma) $ do
    e <- dictCons' tenv tau sigma
    traceD (text "dictCons " <> pretty tau <> text " <: " <> pretty sigma <> " ~> " <>
            align (pretty e))
    pure e

dictCons' :: TyEnv -> G.Type -> G.Type -> T TL.Exp
dictCons' _ tau sigma
  -- TD-CONS-REFL
  | tau == sigma = pure TL.idFun
dictCons' tenv (G.TyVar a) tau = do
  -- TD-CONS-TYVAR
  x <- freshVar
  sigma <- lookupTyVar a tenv
  e <- dictCons tenv sigma tau
  pure $ TL.expAbs (TL.PatVar x)
           (TL.expApp e (TL.expApp (boundFunOfTyvar (var (tyVarVar a))) (var x)))
dictCons' tenv tau1@(G.TyNamed t1 args1) tau2@(G.TyNamed t2 args2) = do
  unlessM (isIface t2) $ failT (prettyS tau1 ++ " is not a subtype of " ++ prettyS tau2)
  k <- classifyTyName t1
  case k of
    TyNameKindBuiltin -> dictConsStructBuiltin tenv tau1 tau2
    TyNameKindStruct -> dictConsStructBuiltin tenv tau1 tau2
    TyNameKindIface -> do
      -- TD-CONS-IFACE-IFACE
      iface1 <- lookupIface t1
      iface2 <- lookupIface t2
      e <- transTypeStar tenv tau2
      eta1 <- inst (if_formals iface1) args1
      eta2 <- inst (if_formals iface2) args2
      xs <- freshVars (length (if_methods iface1))
      y <- freshVar
      let mspecsHave = Map.fromList (zip (G.applyTySubst eta1 (if_methods iface1)) xs)
      dictEntries <- forM (G.applyTySubst eta2 (if_methods iface2)) $ \mspecWanted -> do
        case Map.lookup mspecWanted mspecsHave of
          Just x -> pure (var x)
          Nothing -> failT (prettyS tau1 ++ " is not a subtype of " ++ prettyS tau2)
      pure $
        TL.expAbs (TL.tuplePat [TL.PatWild, TL.PatVar y, matchDictList (map TL.PatVar xs)]) $
        TL.mkTuple [e, var y, mkDictList dictEntries]
dictCons' _ tau1 tau2 = failT (prettyS tau1 ++ " is not a subtype of " ++ prettyS tau2)

dictConsStructBuiltin :: TyEnv -> G.Type -> G.Type -> T TL.Exp
dictConsStructBuiltin tenv tau1@(G.TyNamed t1 _args1) tau2@(G.TyNamed t2 args2) = do
  -- TD-CONS-STRUCT-IFACE
  x <- freshVar
  iface <- lookupIface t2
  e <- transTypeStar tenv tau2
  eta <- inst (if_formals iface) args2
  methodMap <- methodsStructOrBuiltin tenv tau1
  dictEntries <-
      forM (map (G.applyTySubst eta) (if_methods iface)) $ \wantedSpec ->
      case Map.lookup (G.ms_name wantedSpec) methodMap of
        Just (haveSpec, e)
          | haveSpec == wantedSpec -> do
            let y = methodVar (G.ms_name wantedSpec) t1
            pure (TL.expApp (var y) e)
        _ -> do
          failT (prettyS tau1 ++ " is not a subtype of " ++ prettyS tau2 ++
                 " (method ``" ++ prettyS wantedSpec ++
                 "'' not implemented, available methods: "
                 ++ prettyS (Map.toList methodMap) ++ ")")
  pure $ TL.expAbs (TL.PatVar x) $ TL.mkTuple [e, var x, mkDictList dictEntries]
dictConsStructBuiltin _tenv tau1 tau2 =
  failT ("BUG: called dictConsStructBuiltin with invalid arguments tau1=" ++ prettyS tau1 ++
         ", tau2=" ++ prettyS tau2)

tryDictCons :: TyEnv -> G.Type -> G.Type -> T (Maybe TL.Exp)
tryDictCons tyEnv tau sigma = do
  res <- catchT $ dictCons tyEnv tau sigma
  case res of
    Left _ -> pure Nothing
    Right x -> pure (Just x)

--
-- Dictionary destruction
--

-- \Delta |-dictCons \tau \searrow \sigma ~> E
dictDestr :: TyEnv -> G.Type -> G.Type -> T TL.Exp
dictDestr tenv tau sigma =
  withCtx ("dictDestr " ++ prettyS tau ++ " \\ " ++ prettyS sigma) $ do
    e <- dictDestr' tenv tau sigma
    traceD (text "dictDestr " <> pretty tau <> text " \\ " <> pretty sigma <> " ~> " <>
            align (pretty e))
    pure e

dictDestr' :: TyEnv -> G.Type -> G.Type -> T TL.Exp
dictDestr' _tenv tau sigma
  | tau == sigma = do
      traceD (text "types equal" <+> pretty tau <+> text "\\" <+> pretty sigma)
      pure TL.idFun
dictDestr' tenv tau sigma = do
  kindTau <- classifyTy tau
  kindSigma <- classifyTy sigma
  case (kindTau, kindSigma) of
    (TyKindBuiltin _, TyKindBuiltin _) ->
      -- equality already handled
      failT ("Type assertion from " ++ prettyS tau ++ " to " ++ prettyS sigma ++
              " is guaranteed to fail at runtime")
    (TyKindIface _ _, TyKindStruct _ _) -> do
      traceD (text "TD-DESTR-IFACE-STRUCT" <+> pretty tau <+> text "\\" <+> pretty sigma)
      x <- freshVar
      y <- freshVar
      e <- transType tenv sigma
      pure (TL.expAbs (TL.PatVar x) $
            TL.ExpCase (var x)
             [TL.PatClause
               (TL.tuplePat [TL.PatWild, TL.PatVar y, TL.PatWild])
               (TL.matchEq e (TL.fstOfPair (var y)) (var y)
                "Cannot cast value ~a to struct type ~a with runtime type ~a"
                [var x, TL.ExpStr (prettyT sigma), e])])
    (TyKindIface _ _, TyKindIface _ _) -> do
      traceD (text "TD-DESTR-IFACE-IFACE" <+> pretty tau <+> text "\\" <+> pretty sigma)
      e <- transTypeStar tenv sigma
      y <- freshVar
      pure $ TL.expAbs (TL.tuplePat [TL.PatWild, TL.PatVar y, TL.PatWild])
          (TL.expApp (var dynAssertName) (TL.mkPair (var y) e))
    (TyKindIface _ _, TyKindTyVar a) -> do
      traceD (text "TD-DESTR-IFACE-TYVAR" <+> pretty tau <+> text "\\" <+> pretty sigma)
      let xa = tyVarVar a
      y <- freshVar
      pure $ TL.expAbs (TL.tuplePat [TL.PatWild, TL.PatVar y, TL.PatWild]) $
        TL.matchEqFull (typeOfTyvar (var xa)) (TL.fstOfPair (var y))
          (var y) -- True
          (TL.expApp (var dynAssertName) (TL.mkPair (var y) (typeStarOfTyvar (var xa)))) -- False
    (TyKindTyVar a, _) -> do
      traceD (text "TD-DESTR-TYVAR" <+> pretty tau <+> text "\\" <+> pretty sigma)
      tau' <- lookupTyVar a tenv
      e <- dictDestr tenv tau' sigma
      x <- freshVar
      pure $ TL.expAbs (TL.PatVar x) $
        TL.expApp e (TL.expApp (boundFunOfTyvar (var (tyVarVar a))) (var x))
    (TyKindStruct _ _, TyKindStruct _ _) -> do
      traceD (text "TD-DESTR-STRUCT-STRUCT" <+> pretty tau <+> text "\\" <+> pretty sigma)
      y <- freshVar
      e <- transType tenv sigma
      pure $ TL.expAbs (TL.PatVar y)
               (TL.matchEq e (TL.fstOfPair (var y)) (var y)
                  "Cannot cast value ~a to struct type ~a with runtime type ~a"
                  [var y, TL.ExpStr (prettyT sigma), e])
    (TyKindStruct _ _, TyKindIface _ _) -> do
      traceD (text "TD-DESTR-STRUCT-IFACE" <+> pretty tau <+> text "\\" <+> pretty sigma)
      y <- freshVar
      e <- transTypeStar tenv sigma
      pure $ TL.expAbs (TL.PatVar y)
          (TL.expApp (var dynAssertName) (TL.mkPair (var y) e))
    (TyKindStruct _ _, TyKindTyVar a) -> do
      traceD (text "TD-DESTR-STRUCT-TYVAR" <+> pretty tau <+> text "\\" <+> pretty sigma)
      let xa = tyVarVar a
      y <- freshVar
      pure $ TL.expAbs (TL.PatVar y) $
        TL.matchEqFull (typeOfTyvar (var xa)) (TL.fstOfPair (var y))
          (var y) -- True
          (TL.expApp (var dynAssertName) (TL.mkPair (var y) (TL.sndOfPair (var xa)))) -- False
    _ ->
      failT ("Type assertion from " ++ prettyS tau ++ " to " ++ prettyS sigma ++
             " is guaranteed to fail at runtime")

--
-- Subtyping
--

-- \Delta |- \tau <: \sigma
isSubType :: TyEnv -> G.Type -> G.Type -> T Bool
isSubType tenv tau sigma = do
  kTau <- classifyTy tau
  kSigma <- classifyTy sigma
  case (kTau, kSigma) of
    (TyKindTyVar _, TyKindTyVar _) -> pure (tau == sigma)
    (TyKindStruct _ _, TyKindStruct _ _) -> pure (tau == sigma)
    (TyKindBuiltin _, TyKindBuiltin _) -> pure (tau == sigma)
    (_, TyKindIface _ _) -> do
      mSigma <- Set.fromList <$> methods tenv sigma
      mTau <- Set.fromList <$> methods tenv tau
      pure (mSigma `Set.isSubsetOf` mTau)
    _ -> pure False

assertSubType :: TyEnv -> G.Type -> G.Type -> T ()
assertSubType tenv tau sigma = do
  b <- isSubType tenv tau sigma
  unless b $ failT (prettyS tau ++ " is not a subtype of " ++ prettyS sigma)

--
-- Translating expressions
--

-- <\Delta; \Gamma> |-_{exp} e <: \tau ~> E
transExpSub :: TyEnv -> VarEnv -> G.Exp -> G.Type -> T TL.Exp
transExpSub tyEnv varEnv e sigma = do
  (tau, e2) <- transExp tyEnv varEnv e
  e1 <- dictCons tyEnv tau sigma
  let res = TL.expApp e1 e2
  traceD (text "transExpSub " <> pretty e <> " <: " <> pretty sigma <> " ~> " <> align (pretty res))
  pure res

transExp :: TyEnv -> VarEnv -> G.Exp -> T (G.Type, TL.Exp)
transExp tenv venv e = withCtx ("Expression " ++ prettyS e) $
  do res@(tau, resE) <- transExp' tenv venv e
     traceD (text "transExp " <> pretty e <> " : " <> pretty tau <> " ~> " <> align (pretty resE))
     pure res

transExp' :: TyEnv -> VarEnv -> G.Exp -> T (G.Type, TL.Exp)
transExp' _tyEnv varEnv (G.Var x) = do
  -- TD-VAR
  tau <- lookupVar x varEnv
  pure (tau, var (varVar x))
transExp' tyEnv varEnv (G.StructLit tau args) = do
  -- TD-CONS
  assertTypeOk tyEnv tau
  k <- classifyTy tau
  case k of
    TyKindStruct t phi -> do
      str <- lookupStruct t
      e <- transType tyEnv tau
      eta <- inst (st_formals str) phi
      let fields = st_fields str
      when (length args /= length fields) $
        failT ("Invalid number of arguments for construction of struct " ++ prettyS tau)
      es <-
        forM (zip fields args) $ \((_, fieldTy), arg) ->
          transExpSub tyEnv varEnv arg (G.applyTySubst eta fieldTy)
      pure (tau, TL.mkPair e (TL.mkTuple es))
    _ ->
      failT ("Invalid type for constructing a struct: " ++ prettyS tau)
transExp' tyEnv varEnv (G.Select e f) = do
  -- TD-ACCESS
  (tau, eT) <- transExp tyEnv varEnv e
  k <- classifyTy tau
  case k of
    TyKindStruct t phi -> do
      str <- lookupStruct t
      eta <- inst (st_formals str) phi
      let fields = st_fields str
          n = length fields
      xs <- freshVars n
      case List.find (\((f', _), _) -> f == f') (zip fields xs) of
        Nothing -> failT ("Unknown field " ++ prettyS f ++ " for struct " ++ prettyS tau)
        Just ((_, sigma), x) -> do
          let resultTy = G.applyTySubst eta sigma
              resultExp =
                TL.ExpCase eT
                  [TL.PatClause (TL.tuplePat [TL.PatWild, TL.tuplePat (map TL.PatVar xs)])
                                (var x)]
          pure (resultTy, resultExp)
    _ -> failT ("Cannot access field " ++ prettyS f ++ " from non-struct type " ++ prettyS tau)
transExp' tyEnv varEnv (G.MeCall (G.Var (G.VarName "fmt")) (G.MeName me) [] (fmt:args))
  | me `elem` ["Printf", "Sprintf"] = do
  (_tau, fmtT) <- transExp tyEnv varEnv fmt
  fmt <-
    case fmtT of
      TL.ExpStr text -> pure text
      _ -> failT (T.unpack me ++ " requires string literal as first argument, not " ++ prettyS fmtT)
  argsT <- mapM (\e -> transExp tyEnv varEnv e >>= \(_, t) -> pure t) args
  if me == "Printf"
    then pure (tyBuiltinToType TyVoid, TL.printString fmt argsT)
    else pure (tyBuiltinToType TyString, TL.toString fmt argsT)
transExp' tyEnv varEnv (G.MeCall recvExp m actuals args) = do
  (tau, eT) <- transExp tyEnv varEnv recvExp
  k <- classifyTy tau
  case k of
    TyKindBuiltin t ->
      callOnStructOrBuiltin tyEnv varEnv m (tyBuiltinToTyName t) tau eT actuals args
    TyKindStruct tyName _ ->
      callOnStructOrBuiltin tyEnv varEnv m tyName tau eT actuals args
    TyKindIface _ _ -> do
      -- TD-CALL-IFACE
      callOnIface tyEnv varEnv m actuals (tau, tau) eT args
    TyKindTyVar a -> do
      -- TD-CALL-TYVAR
      bound <- lookupTyVar a tyEnv
      callOnIface
        tyEnv varEnv m actuals
        (tau, bound)
        (TL.expApp (boundFunOfTyvar (var (tyVarVar a))) eT)
        args
transExp' tyEnv varEnv assertE@(G.TyAssert exp tau) = do
  -- TD-ASSERT
  assertTypeOk tyEnv tau
  (sigma, e2) <- transExp tyEnv varEnv exp
  kTau <- classifyTy tau
  kSigma <- classifyTy sigma
  -- we now check the stricter rules of the FGG paper
  case (tyKindLike kTau, tyKindLike kSigma) of
    (TyKindLikeStruct, TyKindLikeStruct) ->
      -- FGG disallows casts like e.(t(Int)), where t is a struct and
      -- e has type t(a) for some type variable a. However, such casts can be
      -- useful.
      case matchTypes tau sigma of
        TypeEq ->
          failT ("Assertion " ++ prettyS assertE ++ " is not necessary because " ++ prettyS exp
                 ++ " already has type " ++ prettyS tau)
        TypeNotEq ->
          failT ("Assertion " ++ prettyS assertE ++
                 " will definitely fail at runtime because " ++ prettyS exp ++
                 " has static type " ++ prettyS sigma ++
                 ", which is a struct type different from the struct type " ++ prettyS tau)
        TypeUnifiable -> pure ()
    (TyKindLikeStruct, TyKindLikeIface) -> do
      sigmaBound <- bound tyEnv sigma
      b <- isSubType tyEnv tau sigmaBound
      if b || not (Set.null (G.freeTyVars sigma))
        then pure () -- T-ASSERT_S from the FGG paper
        else failT ("Assertion " ++ prettyS assertE ++ " will definitely fail at runtime because " ++
                    prettyS exp ++ " has static type " ++ prettyS sigma ++ " and " ++
                    prettyS tau ++ " is not a subtype of the bound of this static type")
    (TyKindLikeStruct, TyKindLikeBuiltin) ->
      failT ("Assertion " ++ prettyS assertE ++ " will definitely fail at runtime because " ++
              prettyS exp ++ " has static type " ++ prettyS sigma ++
              " and this builtin type cannot be casted to struct type " ++ prettyS tau)
    (TyKindLikeIface, TyKindLikeIface) ->
      pure () -- T-ASSERT_I from the FGG paper
    (TyKindLikeIface, _) ->
      case kTau of
        TyKindIface _ _ -> do
          b <- isSubType tyEnv sigma tau
          if b
            then failT ("Assertion " ++ prettyS assertE ++ " is unnecessary because " ++
                        prettyS exp ++ " has static type " ++ prettyS sigma ++
                        " and this type is a subtype of " ++ prettyS tau)
           else
             if (Set.null (G.freeTyVars sigma))
               then failT ("Assertion " ++ prettyS assertE ++
                           " will definitely fail at runtime because " ++
                           prettyS exp ++ " has static type " ++ prettyS sigma ++
                           " and this type is not a subtype of " ++ prettyS tau)
               else pure ()
        TyKindTyVar a -> do
          -- not allowed in FGG: we have a cast exp.(alpha) where
          -- alpha is a type variable and exp has type sigma such that
          -- sigma is a struct type (or a builtin type). FGG does not cover this case.
          -- But a can be instantiated with sigma at runtime, so the type assertion
          -- would succeed.
          aBound <- lookupTyVar a tyEnv
          b <- isSubType tyEnv sigma aBound
          if b
            then pure () -- a is possibly sigma at runtime
            else do      -- a cannot be instantiated with sigma
              failT ("Assertion " ++ prettyS assertE ++
                      " will definitely fail at runtime because " ++
                      prettyS exp ++ " has static type sigma = " ++ prettyS sigma ++
                      " but type variable " ++ prettyS a ++
                      " cannot be instantiated with sigma because sigma is not a subtype of "
                      ++ prettyS aBound ++ ", the bound of " ++ prettyS a)
        _ -> failT ("BUG: impossible kind " ++ show kTau)
    (TyKindLikeBuiltin, _) ->
      failT ("Type assertion to builtin type " ++ prettyS tau ++ " not supported")
  e1 <- dictDestr tyEnv sigma tau
  pure (tau, TL.expApp e1 e2)
transExp' tyEnv varEnv (G.FunCall f actuals args) = do
  sig <- lookupFun f
  (argTypes, resType, e) <-
    instMeSigOrFail
      (\detail ->
         "Cannot instantiate function " ++ prettyS f ++ " with type arguments " ++ prettyS actuals
         ++ ": " ++ detail)
      tyEnv sig actuals
  es <-
    checkArgTypes ("Invalid number of arguments for function " ++ prettyS f)
      tyEnv varEnv argTypes args
  let resExp = TL.expAppMany (var (funVar f)) e [TL.mkTuple es]
  pure (resType, resExp)
transExp' tyEnv varEnv (G.BinOp op exp1 exp2) = do
  (tau1, e1) <- transExp tyEnv varEnv exp1
  (tau2, e2) <- transExp tyEnv varEnv exp2
  case Map.lookup op binOpTypes of
    Just m ->
      case Map.lookup (tau1, tau2) m of
        Just resTy -> pure (resTy, TL.ExpBinOp e1 op e2)
        Nothing ->
          failT $
            "Invalid types for binary operator " ++ prettyS op ++ ": " ++ prettyS tau1 ++
            ", " ++ prettyS tau2
    Nothing ->
      failT ("BUG: Unknown binary operator: " ++ show op)
transExp' tyEnv varEnv (G.UnOp Not exp) = do
  (tau, e) <- transExp tyEnv varEnv exp
  when (tau /= tyBuiltinToType TyBool) $
    failT ("Invalid type for not: " ++ prettyS tau)
  pure (tau, TL.ExpUnOp Not e)
transExp' tyEnv varEnv (G.Cond exp1 exp2 exp3) = do
  (tau1, e1) <- transExp tyEnv varEnv exp1
  (tau2, e2) <- transExp tyEnv varEnv exp2
  (tau3, e3) <- transExp tyEnv varEnv exp3
  when (tau1 /= tyBuiltinToType TyBool) $
    failT ("Invalid type for condition of conditional: " ++ prettyS tau1)
  case tau2 == tau3 of
    True -> pure (tau2, TL.ExpCond e1 e2 e3)
    False -> do
      mE2to3 <- tryDictCons tyEnv tau2 tau3
      mE3to2 <- tryDictCons tyEnv tau3 tau2
      case (mE2to3, mE3to2) of
        (Nothing, Nothing) ->
          failT $
            "Incompatible types for branches of conditional: " ++
            prettyS tau2 ++ ", " ++ prettyS tau3
        (Just e2to3, _) ->
          -- tau2 <: tau3
          pure (tau3, TL.ExpCond e1 (TL.expApp e2to3 e2) e3)
        (Nothing, Just e3to2) ->
          -- tau3 <: tau2
          pure (tau2, TL.ExpCond e1 e2 (TL.expApp e3to2 e3))
transExp' _tyEnv _varEnv (G.IntLit i) = pure (tyBuiltinToType TyInt, TL.ExpInt i)
transExp' _tyEnv _varEnv (G.StrLit s) = pure (tyBuiltinToType TyString, TL.ExpStr s)
transExp' _tyEnv _varEnv (G.CharLit c) = pure (tyBuiltinToType TyRune, TL.ExpChar c)
transExp' _tyEnv _varEnv (G.BoolLit b) = pure (tyBuiltinToType TyBool, TL.ExpBool b)

callOnStructOrBuiltin :: TyEnv
                      -> VarEnv
                      -> G.MeName
                      -> G.TyName
                      -> G.Type
                      -> TL.Exp
                      -> [G.Type]
                      -> [G.Exp]
                      -> T (G.Type, TL.Exp)
callOnStructOrBuiltin tyEnv varEnv m tyName tau eT actuals args = do
  -- TD-CALL-STRUCT
  methodMap <- methodsStructOrBuiltin tyEnv tau
  case Map.lookup m methodMap of
    Nothing -> failT ("No implementation of method " ++ prettyS m ++ " for struct " ++ prettyS tau)
    Just (spec, e') -> do
      (argTypes, resType, e'') <-
        instMeSigOrFail
          (\detail ->
            "Cannot instantiate method " ++ prettyS m ++ " for struct " ++ prettyS tau
             ++ " with type arguments " ++ prettyS actuals ++ ": " ++ detail)
          tyEnv (G.ms_sig spec) actuals
      es <-
        checkArgTypes
          ("Invalid number of arguments for method " ++ prettyS m ++ " of struct "
           ++ prettyS tau)
          tyEnv varEnv argTypes args
      let resExp =
            TL.expAppMany (var (methodVar m tyName))
              e' [eT, e'', TL.mkTuple es]
      pure (resType, resExp)

callOnIface ::
  TyEnv
  -> VarEnv
  -> G.MeName
  -> [G.Type]           -- type args of method call
  -> (G.Type, G.Type)   -- (receiver type, its bound)
  -> TL.Exp             -- exp for extracting the dict
  -> [G.Exp]            -- arguments
  -> T (G.Type, TL.Exp)
callOnIface tyEnv varEnv m actuals (recvTy, boundTy) dictE args = do
  mSpecs <- methods tyEnv boundTy
  xs <- freshVars (length mSpecs)
  y <- freshVar
  case List.find (\(spec, _) -> G.ms_name spec == m) (zip mSpecs xs) of
    Nothing -> failT ("Unknown method " ++ prettyS m ++ " for receiver type " ++ prettyS recvTy)
    Just (spec, x) -> do
      (argTypes, resType, e') <-
        instMeSigOrFail
          (\detail ->
             "Cannot instantiate method " ++ prettyS m ++ " for receiver type " ++ prettyS recvTy
              ++ " with type arguments " ++ prettyS actuals ++ ": " ++ detail)
          tyEnv (G.ms_sig spec) actuals
      es <-
        checkArgTypes
          ("Invalid number of arguments for method " ++ prettyS m ++ " of receiverType "
           ++ prettyS recvTy)
          tyEnv varEnv argTypes args
      let resExp =
            TL.ExpCase dictE
              [TL.PatClause
                 (TL.tuplePat [TL.PatWild, TL.PatVar y, matchDictList (map TL.PatVar xs)])
                 (TL.expAppMany (var x) (var y) [e', TL.mkTuple es])]
      pure (resType, resExp)

checkArgTypes :: String -> TyEnv -> VarEnv -> [G.Type] -> [G.Exp] -> T [TL.Exp]
checkArgTypes errMsg tyEnv varEnv argTypes args = do
  when (length args /= length argTypes) $ failT errMsg
  forM (zip args argTypes) $ \(arg, sigma) ->
    transExpSub tyEnv varEnv arg sigma

--
-- Name mapping
--

-- X_{m, t_S}
methodVar :: G.MeName -> G.TyName -> TL.Var
methodVar (G.MeName m) (G.TyName t) = TL.Var ("m-" <> m <> "-" <> t)

-- toplevel functions
funVar :: G.MeName -> TL.Var
funVar (G.MeName f) = TL.Var ("f-" <> f)

-- \alpha ~> X_{\alpha}
tyVarVar :: G.TyVarName -> TL.Var
tyVarVar (G.TyVarName a) = TL.Var ("tv-" <> a)

-- x ~> X
varVar :: G.VarName -> TL.Var
varVar (G.VarName x) = TL.Var x

-- t ~> K_t
constrForTyName :: G.TyName -> TL.Constr
constrForTyName (G.TyName t) = TL.Constr ("Kt-" <> t)

-- m ~> K_m
constrForMeName :: G.MeName -> TL.Constr
constrForMeName (G.MeName m) = TL.Constr ("Km-" <> m)

-- i ~> K_i
constrI :: Int -> TL.Constr
constrI i = TL.Constr ("Ki-" <> showText i)

implName :: TL.Var
implName = "-impl"

unpackName :: TL.Var
unpackName = "-unpack"

dynAssertName :: TL.Var
dynAssertName = "-dyn-assert"

starTyName :: TL.Var
starTyName = "-star-ty"

--
-- Translating declarations and programs
--

data TransDeclRes
  = TransDeclRes
  { tdr_binding :: Maybe TL.Binding
  , tdr_implClause :: Maybe TL.PatClause
  , tdr_starTyClause :: Maybe TL.PatClause
  }
  deriving (Show)

transDecl :: G.Decl -> T TransDeclRes
transDecl decl =
    case decl of
      G.TypeDecl tyName formals tyLit -> withCtx ("declaration of " ++ prettyS tyName) $ do
        -- T-TYPE from FGG paper
        _ <- assertTyFormalsToTyEnv emptyTyFormals formals
        assertTypeDeclOk formals tyLit
        let n = length (G.unTyFormals formals)
        xs <- freshVars n
        tyExpEnv <- mkTyExpEnv (zipWith (\(a, _) x -> (a, var x)) (G.unTyFormals formals) xs)
        tyExp <- transTypeStar' emptyTyEnv tyExpEnv
          (G.TyNamed tyName (map (G.TyVar . fst) (G.unTyFormals formals)))
        let kt = constrForTyName tyName
            patClause = TL.PatClause
                          (TL.PatConstr kt
                            (if n == 0 then [] else [TL.tuplePat (map TL.PatVar xs)]))
                          tyExp
        pure $ TransDeclRes Nothing Nothing (Just patClause)
      G.MeDecl (x, t, tFormals)
                 (G.MeSpec m sig@(G.MeSig mFormals args res))
                 body ->
        -- TD-METHOD
        withCtx ("declaration of method " ++ prettyS m ++ " for " ++ prettyS t) $ do
          k <- classifyTyName t
          tFormals' <-
            case k of
              TyNameKindBuiltin -> pure emptyTyFormals
              TyNameKindStruct -> do
                struct <- lookupStruct t
                pure (st_formals struct)
              TyNameKindIface ->
                failT ("Cannot declare method for interface " ++ prettyS t)
          assertTyFormalsSub tFormals tFormals'
          tenv <- assertTyFormalsToTyEnv tFormals mFormals
          assertTypesOk tenv (res : map snd args)
          let alphas = tyVarsFromFormals tFormals
              betas = tyVarsFromFormals mFormals
          venv <- mkVarEnv ((x, G.TyNamed t (map G.TyVar alphas)) : args)
          tExp <- transMethodBody tenv venv body res
          let tM = methodVar m t
              xAlphas = map tyVarVar alphas
              tX = varVar x
              xBetas = map tyVarVar betas
              tXs = map (varVar . fst) args
              binding = TL.Binding
                            tM
                            [ TL.tuplePat (map TL.PatVar xAlphas)
                            , TL.PatVar tX
                            , TL.tuplePat (map TL.PatVar xBetas)
                            , TL.tuplePat (map TL.PatVar tXs) ]
                            tExp
          -- TD-METHOD-IMPL
          let n = length (G.unTyFormals tFormals)
          x <- freshVar
          y <- freshVar
          xs <- freshVars n
          tyExpEnv <- mkTyExpEnv (zipWith (\(a, _) x -> (a, var x)) (G.unTyFormals tFormals) xs)
          tSigExp <- transMethodSig emptyTyEnv tyExpEnv sig
          es <-
            forM (zip (G.unTyFormals tFormals) xs) $ \((_, bound), xi) -> do
              let tau = maybeType bound
              ei' <- transTypeStar' emptyTyEnv tyExpEnv tau
              pure (TL.mkTriple
                     (var xi)
                     (TL.expApp (var starTyName) (var xi))
                     (TL.expAbs (TL.PatVar y)
                       (TL.expApp (var dynAssertName)
                         (TL.mkPair (TL.expApp (var unpackName) (var y)) ei')))
                   )
          let kt = constrForTyName t
              km = constrForMeName m
              patClause = TL.PatClause
                              (TL.PatConstr
                                TL.pairConstr
                                [ TL.PatConstr kt
                                    (if n == 0 then [] else [TL.tuplePat (map TL.PatVar xs)])
                                , TL.PatConstr km [TL.PatVar x]])
                              (TL.matchEq tSigExp (var x) (TL.expApp (var tM) (TL.mkTuple es))
                               ("found implementation of method ~a for struct ~a with " <>
                                "signature ~a  but this does not match required signature ~a")
                               [TL.ExpConstr km, TL.ExpConstr kt, tSigExp, var x])
              res = TransDeclRes (Just binding) (Just patClause) Nothing
          pure res
      G.FunDecl (G.MeSpec f (G.MeSig mFormals args res)) body ->
        withCtx ("declaration of function " ++ prettyS f) $ do
          tenv <- assertTyFormalsToTyEnv emptyTyFormals mFormals
          assertTypesOk tenv (res : map snd args)
          venv <- mkVarEnv args
          tExp <- transMethodBody tenv venv body res
          let betas = tyVarsFromFormals mFormals
              xBetas = map tyVarVar betas
              tXs = map (varVar . fst) args
              binding = TL.Binding
                            (funVar f)
                            [ TL.tuplePat (map TL.PatVar xBetas)
                            , TL.tuplePat (map TL.PatVar tXs) ]
                            tExp
          pure $ TransDeclRes (Just binding) Nothing Nothing

transMethodBody :: TyEnv -> VarEnv -> G.MeBody -> G.Type -> T TL.Exp
transMethodBody tenv venv body resTy = withCtx "method/function body" $ do
  (varEnv, cont) <- loop venv id (G.mb_bindings body)
  returnE <-
    case G.mb_return body of
      Just mainExp -> transExpSub tenv varEnv mainExp resTy
      Nothing ->
        if G.isVoid resTy
           then pure TL.ExpVoid
           else failT ("Method/function returns nothing but return type is " ++ prettyS resTy)
  pure (cont returnE)
  where
    loop :: VarEnv -> (TL.Exp -> TL.Exp) -> [(G.VarName, Maybe G.Type, G.Exp)] -> T (VarEnv, TL.Exp -> TL.Exp)
    loop varEnv cont [] = pure (varEnv, cont)
    loop varEnv@(VarEnv m) cont ((x, mTy, exp) : rest) = do
      (tau, translatedExp) <-
        case mTy of
          Nothing -> transExp emptyTyEnv varEnv exp
          Just ty -> do
            exp' <- transExpSub emptyTyEnv varEnv exp ty
            pure (ty, exp')
      let newCont e = cont $ TL.ExpCase translatedExp [TL.PatClause (TL.PatVar (varVar x)) e]
      loop (VarEnv (Map.insert x tau m)) newCont rest

transMain :: G.MeBody -> T TL.Exp
transMain main = withCtx "main function" $ do
  e <- transMethodBody emptyTyEnv emptyVarEnv main G.tyVoid
  pure e

transProg :: G.Program -> T TL.Prog
transProg prog = do
  trace $ T.pack ("source program: " ++ show prog)
  declRes <- mapM transDecl (G.p_decls prog)
  x <- freshVar
  let bindings :: [TL.Binding]
      bindings = mapMaybe tdr_binding declRes
      implClauses :: [TL.PatClause]
      implClauses = mapMaybe tdr_implClause declRes ++ [defaultImplClause x]
      implBinding =
        TL.Binding implName [TL.PatVar x] $
          TL.ExpCase (var x) implClauses
      starTyClauses :: [TL.PatClause]
      starTyClauses = mapMaybe tdr_starTyClause declRes
      starTyBinding =
        TL.Binding starTyName [TL.PatVar x] $
          TL.ExpCase (var x) (starTyClauses ++ [catchAllStarTyClause x])
  mainEs <- mapM transMain (G.p_mains prog)
  pure (TL.Prog (implBinding : starTyBinding : bindings) mainEs)
  where
    defaultImplClause x =
      TL.PatClause TL.PatWild (TL.ExpFail "no method implementation for ~a" [var x])
    catchAllStarTyClause x =
      TL.PatClause TL.PatWild (var x) -- handles builtin types

runTrans :: G.Program -> (Either TransError TL.Prog, [T.Text])
runTrans prog = genRunTrans cfg prog transProg
  where
    cfg =
      TransConfig
      { tc_freshVarPrefix = "x-"
      , tc_assertSubType = assertSubType
      , tc_checkInst = \tenv formals args -> do
          mx <- tryInst tenv formals args
          case mx of
            Left err -> pure (Left err)
            Right _ -> pure (Right ())
      }

runTranslation' :: G.Program -> (Either String TL.Prog, [T.Text])
runTranslation' = runTrans

-- Terminates the program if type checking fails
runTranslation :: TraceFlag -> T.Text -> FilePath -> G.Program -> IO T.Text
runTranslation traceFlag header filePath prog = do
  let (result, trace) = runTrans prog
  case traceFlag of
    TraceOn -> outputTrace trace
    TraceOff -> pure ()
  tProg <-
    case result of
      Right p -> pure p
      Left err -> do
        hPutStrLn stderr ("Typechecking " ++ filePath ++ " failed: " ++ err)
        exitWith (ExitFailure 1)
  pure (header <> TL.translateProg stdlibForTrans tProg)

stdlibForTrans :: T.Text
stdlibForTrans = [r|
(define (iface-sigs? x)
  (and (list? x) (not (null? x)) (equal? (car x) '-sig)))

(define (-tyrep x)
  (cond
   [(number? x) 'Kt-int]
   [(char? x) 'Kt-rune]
   [(boolean? x) 'Kt-bool]
   [(string? x) 'Kt-string]
   [else
    (match
     x
     ((list 'tuple-2 tyrep _) tyrep)
     (_ (error 'ERROR "malformed value ~a" x)))]))

(define (-unpack x)
  (match
   x
   ((list 'tuple-3 _ val _) val)
   (val val)))

(define (-dyn-assert x)
  (match
   x
   ((list 'tuple-2 val sigs)
    (if (iface-sigs? sigs)
        ;; sigs is a pair (improper list), so we need cadr
        (list 'tuple-3 sigs val (-impls (-tyrep val) (cadr sigs)))
        (error 'ERROR "cannot cast value ~a to runtime type ~a" val sigs)))))

(define (-impls tyrep sigs)
  (if (null? sigs)
      '()
      (cons (-impl (list 'tuple-2 tyrep (car sigs))) (-impls tyrep (cdr sigs)))))
|]
