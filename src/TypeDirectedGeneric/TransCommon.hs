{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
module TypeDirectedGeneric.TransCommon where

import Common.Utils
import Common.PrettyUtils
import Common.Types
import qualified Common.FGGAST as G
import Common.FGGPretty ()
import Prettyprinter

import qualified Data.Set as Set
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.DList as DL
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS.Strict
import qualified Data.Text as T
import qualified Data.List as List
import Data.Maybe

--
-- Predefined types
--

tyAny :: G.Type
tyAny = G.TyNamed "Any" []

--
-- Access to various construct of the source program
--

data TyBuiltin
  = TyInt
  | TyRune
  | TyBool
  | TyString
  | TyVoid
  deriving (Show)

data TyKind
  = TyKindBuiltin TyBuiltin
  | TyKindTyVar G.TyVarName
  | TyKindStruct G.TyName [G.Type]
  | TyKindIface G.TyName [G.Type]
  deriving (Show)

data TyKindLike
  = TyKindLikeStruct
  | TyKindLikeBuiltin
  | TyKindLikeIface
  deriving (Show)

tyKindLike :: TyKind -> TyKindLike
tyKindLike (TyKindBuiltin _) = TyKindLikeBuiltin
tyKindLike (TyKindTyVar _) = TyKindLikeIface
tyKindLike (TyKindIface _ _) = TyKindLikeIface
tyKindLike (TyKindStruct _ _) = TyKindLikeStruct

data TyNameKind
  = TyNameKindBuiltin
  | TyNameKindStruct
  | TyNameKindIface

data MeDecl
    = MeDecl
    { me_recv :: G.VarName
    , me_tyName :: G.TyName
    , me_formals :: G.TyFormals
    , me_spec :: G.MeSpec
    , me_exp :: G.MeBody
    }
  deriving Show

data Iface
  = Iface
  { _if_tyName :: G.TyName
  , if_formals :: G.TyFormals
  , if_methods :: [G.MeSpec]
  }

data Struct
  = Struct
  { _st_tyName :: G.TyName
  , st_formals :: G.TyFormals
  , st_fields :: [(G.FieldName, G.Type)]
  }

allMethodsForStructOrBuiltin :: G.TyName -> T [MeDecl]
allMethodsForStructOrBuiltin t = do
  env <- ask
  case Map.lookup t (te_methods env) of
    Just l -> pure l
    Nothing -> pure []

findOrFail :: (Pretty k, Ord k) => String -> (TransEnv -> Map k a) -> k -> T a
findOrFail what fun k = do
  env <- ask
  case Map.lookup k (fun env) of
    Just x -> pure x
    Nothing -> failT ("Unknown " ++ what ++ ": " ++ prettyS k)

isIn :: Ord k => (TransEnv -> Map k a) -> k -> T Bool
isIn fun k = do
  env <- ask
  pure (isJust (Map.lookup k (fun env)))

lookupIface :: G.TyName -> T Iface
lookupIface = findOrFail "interface" te_ifaces

lookupStruct :: G.TyName -> T Struct
lookupStruct = findOrFail "struct" te_structs

lookupFun :: G.MeName -> T G.MeSig
lookupFun = findOrFail "function" te_funs

isIface :: G.TyName -> T Bool
isIface = isIn te_ifaces

_isStruct :: G.TyName -> T Bool
_isStruct = isIn te_structs

--
-- Translation monad
--

data TransConfig
  = TransConfig
  { tc_freshVarPrefix :: T.Text
  , tc_assertSubType :: TyEnv -> G.Type -> G.Type -> T ()
  , tc_checkInst :: TyEnv -> G.TyFormals -> [G.Type] -> T (Either String ())
  }

getConfig :: T TransConfig
getConfig = do
  e <- ask
  pure (te_cfg e)

data TransEnv
    = TransEnv
    { te_structs :: Map G.TyName Struct
    , te_ifaces :: Map G.TyName Iface
    , te_funs :: Map G.MeName G.MeSig
    , te_methods :: Map G.TyName [MeDecl] -- grouped by receiver (either struct or builtin)
    , te_cfg :: TransConfig
    }

emptyTransEnv :: TransConfig -> TransEnv
emptyTransEnv cfg =
  TransEnv
  { te_structs = Map.empty
  , te_ifaces = Map.empty
  , te_funs = Map.empty
  , te_methods = Map.empty
  , te_cfg = cfg
  }

data TransState
    = TransState
    { ts_nextFreshVar :: Int
    , ts_contextStack :: [String]
    }

type TransError = String

type TransTracer = Writer (DL.DList T.Text) -- trace in reverse

trace :: T.Text -> T ()
trace t = T $ (lift . lift . tell) (DL.fromList [t])

traceD :: Doc a -> T ()
traceD d = trace (T.pack (show d))

newtype T a = T { unT :: RWST TransEnv () TransState (ExceptT TransError TransTracer) a }
    deriving ( Functor, Applicative, Monad, MonadReader TransEnv, MonadState TransState
             , MonadError String)

genFreshVar :: T T.Text
genFreshVar = do
  s <- get
  let i = ts_nextFreshVar s
  put (s { ts_nextFreshVar = i + 1 })
  env <- ask
  let pref = tc_freshVarPrefix (te_cfg env)
  return $ pref <> showText i

genFreshVars :: Int -> T [T.Text]
genFreshVars n = mapM (\_ -> genFreshVar) [1..n]

builtinTypes :: Set.Set G.TyName
builtinTypes = Set.fromList ["int", "bool", "rune", "string", "void"]

tyBuiltinToTyName :: TyBuiltin -> G.TyName
tyBuiltinToTyName b =
  case b of
    TyInt -> "int"
    TyRune -> "rune"
    TyBool -> "bool"
    TyString -> "string"
    TyVoid -> "void"

tyBuiltinToType :: TyBuiltin -> G.Type
tyBuiltinToType b = G.TyNamed (tyBuiltinToTyName b) []

classifyTyName :: G.TyName -> T TyNameKind
classifyTyName t = do
  env <- ask
  pure $ if | t `Map.member` te_structs env -> TyNameKindStruct
            | t `Set.member` builtinTypes -> TyNameKindBuiltin
            | otherwise -> TyNameKindIface

classifyTy :: G.Type -> T TyKind
classifyTy tau = do
  env <- ask
  case tau of
    G.TyVar a -> pure (TyKindTyVar a)
    G.TyNamed t args ->
      case (Map.lookup t (te_structs env), Map.lookup t (te_ifaces env)) of
        (Just _, Nothing) -> pure (TyKindStruct t args)
        (Nothing, Just _) -> pure (TyKindIface t args)
        (Just _, Just _) ->
          failT ("BUG: type name " ++ prettyS t ++ " is both a struct and an interface?!")
        (Nothing, Nothing)
          | t == "int" -> pure $ TyKindBuiltin TyInt
          | t == "bool" -> pure $ TyKindBuiltin TyBool
          | t == "rune" -> pure $ TyKindBuiltin TyRune
          | t == "string" -> pure $ TyKindBuiltin TyString
          | t == "void" -> pure $ TyKindBuiltin TyVoid
          | otherwise -> failT ("Unknown type name " ++ prettyS t)

withCtx :: String -> T a -> T a
withCtx c action = do
  modify' (\s -> s { ts_contextStack = c : ts_contextStack s })
  trace (T.pack $ "Starting " ++ c)
  x <- action
  trace (T.pack $ "Finished " ++ c)
  modify' (\s -> s { ts_contextStack = tail (ts_contextStack s) })
  pure x

failT :: String -> T a
failT msg = do
  stack <- gets ts_contextStack
  case stack of
    [] -> throwError msg
    _ ->
      throwError (msg ++ "\nContext:\n" ++ unlines (map (\s -> "  " ++ s) stack))

catchT :: T a -> T (Either TransError a)
catchT (T action) =
    T $
    (action >>= \x -> pure (Right x)) `catchError` (\e -> pure (Left e))

--
-- Environments
--

newtype TyEnv = TyEnv { unTyEnv :: Map G.TyVarName G.Type }
    deriving (Show)

emptyTyEnv :: TyEnv
emptyTyEnv = TyEnv Map.empty

lookupTyVar :: G.TyVarName -> TyEnv -> T G.Type
lookupTyVar a (TyEnv m) =
    case Map.lookup a m of
      Just t -> pure t
      Nothing -> failT ("Unbound type variable " ++ prettyS a)

emptyTyFormals :: G.TyFormals
emptyTyFormals = G.TyFormals []

newtype GenTyExpEnv exp = GenTyExpEnv { _unTyExpEnv :: Map G.TyVarName exp }
    deriving (Show)

emptyTyExpEnv :: GenTyExpEnv exp
emptyTyExpEnv = GenTyExpEnv Map.empty

mkTyExpEnv :: [(G.TyVarName, exp)] -> T (GenTyExpEnv exp)
mkTyExpEnv l = do
  assertDistinct (map fst l)
  pure $ GenTyExpEnv (Map.fromList l)

extendTyExpEnv :: GenTyExpEnv exp -> [(G.TyVarName, exp)] -> T (GenTyExpEnv exp)
extendTyExpEnv (GenTyExpEnv m1) l = do
  GenTyExpEnv m2 <- mkTyExpEnv l
  pure (GenTyExpEnv (m1 `Map.union` m2)) -- union is left-biased

newtype VarEnv = VarEnv { _unVarEnv :: Map G.VarName G.Type }

emptyVarEnv :: VarEnv
emptyVarEnv = VarEnv Map.empty

lookupVar :: G.VarName -> VarEnv -> T G.Type
lookupVar x (VarEnv m) =
    case Map.lookup x m of
      Just t -> pure t
      Nothing -> failT ("Unbound variable " ++ prettyS x)

mkVarEnv :: [(G.VarName, G.Type)] -> T VarEnv
mkVarEnv l = do
  assertDistinct (map fst l)
  pure $ VarEnv (Map.fromList l)

extendVarEnv :: VarEnv -> [(G.VarName, G.Type)] -> T VarEnv
extendVarEnv (VarEnv old) l = do
  VarEnv new <- mkVarEnv l
  pure (VarEnv (new `Map.union` old)) -- union is left-biased

--
-- Auxiliary functions
--

tyVarsFromFormals :: G.TyFormals -> [G.TyVarName]
tyVarsFromFormals (G.TyFormals l) = map fst l

tyFormalsToTyEnv :: G.TyFormals -> TyEnv
tyFormalsToTyEnv phi = TyEnv $ Map.fromList (map setBound (G.unTyFormals phi))
  where
    setBound (a, mt) = (a, maybeType mt)

joinTyEnvs :: TyEnv -> TyEnv -> TyEnv
joinTyEnvs (TyEnv first) (TyEnv second) = TyEnv $ merge preserveMissing preserveMissing (zipWithMatched (\_ _ y -> y)) first second

maybeType :: Maybe G.Type -> G.Type
maybeType (Just t) = t
maybeType Nothing = tyAny

binOpTypes :: Map BinOp (Map (G.Type, G.Type) G.Type)
binOpTypes =
  Map.fromList $
  flip map table $ \(op, types) ->
    (op, Map.fromList (map (\(t1, t2, r) -> ((f t1, f t2), f r)) types))
  where
    f = tyBuiltinToType
    table :: [(BinOp, [(TyBuiltin, TyBuiltin, TyBuiltin)])]
    table = [
      (Plus, [(TyInt, TyInt, TyInt), (TyString, TyString, TyString)])
      , (Minus, [(TyInt, TyInt, TyInt)])
      , (Mult, [(TyInt, TyInt, TyInt)])
      , (Div, [(TyInt, TyInt, TyInt)])
      , (Mod, [(TyInt, TyInt, TyInt)])
      , (Lt, [(TyInt, TyInt, TyBool)])
      , (Gt, [(TyInt, TyInt, TyBool)])
      , (LtEqual, [(TyInt, TyInt, TyBool)])
      , (GtEqual, [(TyInt, TyInt, TyBool)])
      , (Equal, [ (TyInt, TyInt, TyBool)
                , (TyRune, TyRune, TyBool)
                , (TyBool, TyBool, TyBool)
                , (TyString, TyString, TyBool)
                ])
      , (NotEqual, [ (TyInt, TyInt, TyBool)
                   , (TyRune, TyRune, TyBool)
                   , (TyBool, TyBool, TyBool)
                   , (TyString, TyString, TyBool)
                   ])
      , (And, [(TyBool, TyBool, TyBool)])
      , (Or, [(TyBool, TyBool, TyBool)])
      ]

--
-- Checking well-formedness
--

-- \Phi |- \Psi ok
assertTyFormalsOk :: G.TyFormals -> G.TyFormals -> T ()
assertTyFormalsOk phi psi = do
  -- T-FORMAL
  assertDistinct (tyVarsFromFormals phi ++ tyVarsFromFormals psi)
  let tenv = tyFormalsToTyEnv (phi <> psi)
      tys = mapMaybe snd (G.unTyFormals psi)
  assertTypesOk tenv tys
  forM_ tys assertIsIface
  where
    assertIsIface tau = do
      k <- classifyTy tau
      case k of
        TyKindIface _ _ -> pure ()
        _ -> failT ("Upper bound " <> prettyS tau <> " must be an interface type")

-- \Phi; \Psi ok \Delta
assertTyFormalsToTyEnv :: G.TyFormals -> G.TyFormals -> T TyEnv
assertTyFormalsToTyEnv phi psi = do
  -- T-NESTED
  assertTyFormalsOk emptyTyFormals phi
  assertTyFormalsOk phi psi
  pure $ tyFormalsToTyEnv (phi <> psi)

-- \Phi <: \Psi
assertTyFormalsSub :: G.TyFormals -> G.TyFormals -> T ()
assertTyFormalsSub (G.TyFormals phi) (G.TyFormals psi) = do
  -- <:-FORMALS
  when (length phi /= length psi) $
    failT (prettyS phi ++ " has not the same number of type variables as " ++ prettyS psi)
  let subst = Map.fromList (zipWith (\(a, _) (b, _) -> (a, G.TyVar b)) phi psi)
      taus = map (\(_, t) -> maybeType (G.applyTySubst subst t)) phi
      sigmas = map (maybeType . snd) psi
  cfg <- getConfig
  forM_ (zip taus sigmas) $ \(tau, sigma) -> tc_assertSubType cfg emptyTyEnv tau sigma

-- \Phi |- L ok
assertTypeLitOk :: G.TyFormals -> G.TyLit -> T ()
assertTypeLitOk phi lit =
  case lit of
    -- T-STRUCT
    G.Struct fields -> do
      assertDistinct (map fst fields)
      assertTypesOk tenv (map snd fields)
    G.Iface methods -> do
      -- T-INTERFACE
      assertDistinct (map G.ms_name methods)
      forM_ methods $ assertMeSpecOk phi
    G.TySyn t -> assertTypeOk tenv t
  where
    tenv = tyFormalsToTyEnv phi

-- \Phi |- S ok
assertMeSpecOk :: G.TyFormals -> G.MeSpec -> T ()
assertMeSpecOk phi (G.MeSpec _ (G.MeSig psi args res)) = do
  -- T-SPECIFICATION
  tenv <- assertTyFormalsToTyEnv phi psi
  assertDistinct (map fst args)
  assertTypesOk tenv (res : map snd args)

assertTypeDeclOk :: G.TyFormals -> G.TyLit -> T ()
assertTypeDeclOk phi lit = do
  -- T-TYPE
  assertTyFormalsOk emptyTyFormals phi
  assertTypeLitOk phi lit

assertDistinct :: (Ord a, Pretty a) => [a] -> T ()
assertDistinct l = loop Set.empty l
  where
    loop _ [] = pure ()
    loop found (x:xs) =
        if x `Set.member` found
        then failT ("Duplicate in " ++ prettyS (CommaList l) ++ ": " ++ prettyS x)
        else loop (Set.insert x found) xs

assertTypeOk :: TyEnv -> G.Type -> T ()
assertTypeOk tenv tau = withCtx ("checking that type " ++ prettyS tau ++ " is OK") $ do
  k <- classifyTy tau
  case k of
    TyKindBuiltin _ -> pure ()
    TyKindTyVar a ->
      if a `Map.member` unTyEnv tenv
      then pure ()
      else failT ("Unbound type variable " ++ prettyS a)
    TyKindStruct t args -> do
      struct <- lookupStruct t
      checkTyWithArgs (st_formals struct) args
    TyKindIface t args -> do
      iface <- lookupIface t
      checkTyWithArgs (if_formals iface) args
  where
    checkTyWithArgs formals args = do
      assertTypesOk tenv args
      cfg <- getConfig
      mx <- tc_checkInst cfg tenv formals args
      case mx of
        Left err ->
          failT ("Type arguments do not respect the bounds in " ++ prettyS tau ++ ": " ++ err)
        Right () -> assertTypesOk tenv args

assertTypesOk :: TyEnv -> [G.Type] -> T ()
assertTypesOk tenv ts = mapM_ (assertTypeOk tenv) ts

--
-- Auxiliary judgments
--

bound :: TyEnv -> G.Type -> T G.Type
bound tenv t =
  case t of
    G.TyVar a -> do
      lookupTyVar a tenv
    _ -> pure t

getBound :: TyEnv -> G.TyVarName -> T G.Type
getBound tenv a =
  case Map.lookup a (unTyEnv tenv) of
    Just t -> pure t
    Nothing -> failT ("Type variable " ++ prettyS a ++ " not found in type environment")

--
-- Running the translation
--
genRunTrans :: forall a . TransConfig -> G.Program -> (G.Program -> T a) -> (Either TransError a, [T.Text])
genRunTrans cfg prog transProg =
  case List.foldl' addToEnv (Right (emptyTransEnv cfg)) (G.p_decls prog) of
    Left err -> (Left err, [])
    Right env ->
      let state = TransState 0 []
          result :: (Either TransError (a, ()), DL.DList T.Text)
          result =
            runWriter $ runExceptT $ evalRWST (unT $ transProg prog) env state
      in case result of
           (Left err, trace) -> (Left err, DL.toList trace)
           (Right (prog, ()), trace) -> (Right prog, DL.toList trace)
  where
    addToEnv :: Either TransError TransEnv -> G.Decl -> Either TransError TransEnv
    addToEnv (Left err) _ = Left err
    addToEnv (Right env) decl =
      case decl of
        G.TypeDecl t formals (G.Struct fields) -> do
          checkExists t (te_ifaces env)
          structs <- addIfNotExists t (Struct t formals fields) (te_structs env)
          Right (env { te_structs = structs })
        G.TypeDecl _t _formals (G.TySyn _ty) -> do
          Left "type synonyms are not supported in the translation for generics"
        G.TypeDecl t formals (G.Iface methods) -> do
          checkExists t (te_structs env)
          ifaces <- addIfNotExists t (Iface t formals methods) (te_ifaces env)
          Right (env { te_ifaces = ifaces })
        G.MeDecl (recvVar, recvTy, formals) spec exp -> do
          let oldMethods = Map.findWithDefault [] recvTy (te_methods env)
              m = G.ms_name spec
          case List.find (\mDecl -> G.ms_name (me_spec mDecl) == m) oldMethods of
            Just _ ->
              Left $
                "Duplicate method declaration for receiver " ++ prettyS recvTy ++
                " and method " ++ prettyS m
            Nothing ->
              let newMethod =
                    MeDecl
                      { me_recv = recvVar
                      , me_tyName = recvTy
                      , me_formals = formals
                      , me_spec = spec
                      , me_exp = exp
                      }
              in Right $
                 env { te_methods = Map.insert recvTy (newMethod : oldMethods) (te_methods env) }
        G.FunDecl spec _exp -> do
          funs <- addIfNotExists (G.ms_name spec) (G.ms_sig spec) (te_funs env)
          Right (env { te_funs = funs })

checkExists :: (Pretty k, Ord k) => k -> Map k a -> Either TransError ()
checkExists k m =
  case Map.lookup k m of
    Nothing -> Right ()
    Just _ -> Left ("Duplicate declaration for " ++ prettyS k)

addIfNotExists :: (Pretty k, Ord k) => k -> a -> Map k a -> Either TransError (Map k a)
addIfNotExists k a m =
  case Map.lookup k m of
    Nothing -> Right (Map.insert k a m)
    Just _ -> Left ("Duplicate declaration for " ++ prettyS k)
