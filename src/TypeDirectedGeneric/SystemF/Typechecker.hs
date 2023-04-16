{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.SystemF.Typechecker (

  runT, typeCheck, htf_thisModulesTests

  ) where

import Common.Types
import Common.Utils
import Common.PrettyUtils
import TypeDirectedGeneric.SystemF.Syntax
import TypeDirectedGeneric.SystemF.Pretty ()

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.DList as DL
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Text as T
import Data.Generics
import Test.Framework

newtype TySubst = TySubst { _unTySubst :: Map TyVarName Ty }

singleTySubst :: TyVarName -> Ty -> TySubst
singleTySubst a ty = TySubst (M.singleton a ty)

mkTySubst :: [TyVarName] -> [Ty] -> String -> T TySubst
mkTySubst tyvars tys err =
  if length tyvars /= length tys
  then failT err
  else pure $ TySubst (M.fromList (zip tyvars tys))

applyTySubst :: TySubst -> Ty -> Ty
applyTySubst s@(TySubst subst) ty =
  let allTyVars = M.keys subst ++ (listify matchTyVarName (ty : M.elems subst))
      nextFresh =
        case mapMaybe freshTyVarCounter allTyVars of
          [] -> 0
          l -> 1 + maximum l
  in evalState (applyTySubstM s ty) nextFresh
  where
    matchTyVarName :: TyVarName -> Bool
    matchTyVarName _ = True

freshTyVarCounter :: TyVarName -> Maybe Int
freshTyVarCounter (TyVarName t) =
  case T.unpack t of
    '-':'A':'-':rest -> Just (read rest) -- must match name produced by freshTyVar
    _ -> Nothing

freshTyVar :: MonadState Int m => m TyVarName
freshTyVar = do
  i <- get
  put (i+1)
  pure (TyVarName (T.pack ("-A-" ++ show i))) -- must match name consumed by freshTyVarCounter

applyTySubstM :: MonadState Int m => TySubst -> Ty -> m Ty
applyTySubstM s@(TySubst subst) ty =
  case ty of
    TyVar a -> pure (M.findWithDefault ty a subst)
    TyPrim _ -> pure ty
    TyArrow ty1 ty2 -> do
      ty1' <- applyTySubstM s ty1
      ty2' <- applyTySubstM s ty2
      pure (TyArrow ty1' ty2')
    TyConstr c tys -> do
      tys' <- mapM (applyTySubstM s) tys
      pure (TyConstr c tys')
    TyForall a ty -> do
      a' <- freshTyVar
      let s' = TySubst (M.insert a (TyVar a') subst)
      ty' <- applyTySubstM s' ty
      pure $ TyForall a' ty'

test_applyTySubst :: IO ()
test_applyTySubst = do
  let ty = TyForall "A" (TyForall "B" (TyArrow "C" (TyArrow "A" "B")))
      subst = TySubst (M.fromList [("A", TyPrim PrimInt), ("C", "B")])
      expectedTy = TyForall "-A-0" (TyForall "-A-1" (TyArrow "B" (TyArrow "-A-0" "-A-1")))
  assertEqual expectedTy (applyTySubst  subst ty)

type TyCheckError = String

type TyCheckTracer = Writer (DL.DList T.Text) -- trace in reverse

type DeclEnv = Map ConstrName ([TyVarName], [Ty])
type TyVarEnv = Set TyVarName
type VarEnv = Map VarName Ty

data TyCheckEnv
  = TyCheckEnv
  { tce_decls :: DeclEnv
  , tce_tyvars :: TyVarEnv
  , tce_vars :: VarEnv
  }

getDecl :: ConstrName -> T ([TyVarName], [Ty])
getDecl c = do
  m <- asks tce_decls
  case M.lookup c m of
    Nothing -> failT ("Unknown constructor: " ++ prettyS c)
    Just x -> pure x

readTyvars :: T TyVarEnv
readTyvars = asks tce_tyvars

readVars :: T VarEnv
readVars = asks tce_vars

withNewVar :: VarName -> Ty -> T a -> T a
withNewVar x ty comp =
  local (\env -> env { tce_vars = M.insert x ty (tce_vars env) }) comp

withNewVars :: VarEnv -> T a -> T a
withNewVars varEnv comp =
  local (\env -> env { tce_vars = M.union varEnv (tce_vars env) }) comp

withNewTyVar :: TyVarName -> T a -> T a
withNewTyVar a comp =
  local (\env -> env { tce_tyvars = S.insert a (tce_tyvars env) }) comp

withNewTyVars :: [TyVarName] -> T a -> T a
withNewTyVars as comp =
  local (\env -> env { tce_tyvars = S.union (S.fromList as) (tce_tyvars env) }) comp

newtype T a = T { unT :: RWST TyCheckEnv () () (ExceptT TyCheckError TyCheckTracer) a }
    deriving ( Functor, Applicative, Monad, MonadReader TyCheckEnv, MonadState ()
             , MonadError String)

runT :: T a -> Either String a
runT action =
  let result = runWriter $ runExceptT $ evalRWST (unT action) emptyTyCheckEnv ()
  in case result of
       (Left err, _trace) -> Left err
       (Right (x, ()), _trace) -> Right x

failT :: String -> T a
failT = throwError

checkTyOk :: Ty -> T ()
checkTyOk ty =
  case ty of
    TyVar a -> do
      tenv <- readTyvars
      unless (a `S.member` tenv) $
        failT ("Unbound type variable " ++ prettyS a)
    TyPrim _ -> pure ()
    TyArrow ty1 ty2 -> do
      checkTyOk ty1
      checkTyOk ty2
    TyConstr c tys -> do
      forM_ tys checkTyOk
      (tyvars, _) <- getDecl c
      when (length tys /= length tyvars) $
        failT ("Type arity mismatch: " ++ prettyS ty)
    TyForall a ty ->
      withNewTyVar a (checkTyOk ty)

checkExp :: Exp -> Ty -> T ()
checkExp e tyExpected = do
  ty <- tyOfExp e
  when (ty /= tyExpected) $
    failT ("Expression " ++ prettyS e ++ " should have type " ++ prettyS tyExpected
          ++ " but has type " ++ prettyS ty)

tyOfExp :: Exp -> T Ty
tyOfExp e = do
  case e of
    ExpVar x -> do
      venv <- readVars
      case M.lookup x venv of
        Nothing -> failT ("Unbound variable: " ++ prettyS x)
        Just t -> pure t
    ExpConstr c tyargs eargs -> do
      mapM_ checkTyOk tyargs
      (tyvars, fieldTys) <- getDecl c
      tySubst <-
        mkTySubst tyvars tyargs ("Type arity mismatch in constructor application: " ++ prettyS e)
      when (length eargs /= length fieldTys) $
        failT ("Arity mismatch in constructor application: " ++ prettyS e)
      forM_ (zip fieldTys eargs) $ \(t, arg) -> checkExp arg (applyTySubst tySubst t)
      pure (TyConstr c tyargs)
    ExpApp e1 e2 -> do
      ty1 <- tyOfExp e1
      ty2 <- tyOfExp e2
      case ty1 of
        TyArrow tyDom tyRes ->
          if ty2 == tyDom
          then pure tyRes
          else failT ("Function of type " ++ prettyS ty1 ++ " applied to value of type " ++
                       prettyS ty2)
        _ ->
          failT ("Left-hand side of function application has not a function type but " ++
                 prettyS ty1 ++ "\n\n" ++ prettyS e)
    ExpAbs x ty e -> do
      checkTyOk ty
      tyRes <- withNewVar x ty (tyOfExp e)
      pure (TyArrow ty tyRes)
    ExpTyApp e tyArg -> do
      checkTyOk tyArg
      tyFun <- tyOfExp e
      case tyFun of
        TyForall a ty -> pure (applyTySubst (singleTySubst a tyArg) ty)
        _ -> failT ("Left-hand side of type application is not a forall type: " ++ prettyS tyFun)
    ExpTyAbs a e -> do
      tyRes <- withNewTyVar a (tyOfExp e)
      pure (TyForall a tyRes)
    ExpCase _ [] -> failT ("Case with no clauses")
    ExpCase e clauses -> do
      ty <- tyOfExp e
      clauseTys <- forM clauses (tyOfClause ty)
      if not (allEq clauseTys)
        then failT ("Clauses of case have different types: " ++ prettyS clauseTys)
        else pure (head clauseTys)
    ExpBinOp e1 op e2 -> do
      ty1 <- tyOfExp e1
      ty2 <- tyOfExp e2
      let (tyArg, tyRes) = tyOfBinOp op
      when (ty1 /= tyArg || ty2 /= tyArg) $
        failT ("Invalid use of binary operator: " ++ prettyS e)
      pure tyRes
    ExpUnOp op e1 -> do
      ty1 <- tyOfExp e1
      let (tyArg, tyRes) = tyOfUnOp op
      when (ty1 /= tyArg) $
        failT ("Invalid use of unary operator: " ++ prettyS e)
      pure tyRes
    ExpCond e1 e2 e3 -> do
      ty1 <- tyOfExp e1
      unless (ty1 == TyPrim PrimBool) $
        failT ("Condition of if has type " ++ prettyS ty1)
      ty2 <- tyOfExp e2
      ty3 <- tyOfExp e3
      unless (ty2 == ty3) $
        failT ("Branches of if have different types: " ++ prettyS ty2 ++ " and " ++ prettyS ty3)
      pure ty2
    ExpInt _ -> pure $ TyPrim PrimInt
    ExpBool _ -> pure $ TyPrim PrimBool
    ExpStr _ -> pure $ TyPrim PrimString
    ExpChar _ -> pure $ TyPrim PrimChar
    ExpFail _ _ -> let a = TyVarName "a" in pure $ TyForall a (TyVar a)
    ExpVoid -> pure $ TyPrim PrimVoid

int :: Ty
int = TyPrim PrimInt

bool :: Ty
bool = TyPrim PrimBool

tyOfBinOp :: BinOp -> (Ty, Ty)
tyOfBinOp op =
  case op of
    Plus -> (int, int)
    Minus -> (int, int)
    Mult -> (int, int)
    Div -> (int, int)
    Mod -> (int, int)
    Equal -> (int, bool)
    NotEqual -> (int, bool)
    Lt -> (int, bool)
    LtEqual -> (int, bool)
    Gt -> (int, bool)
    GtEqual -> (int, bool)
    And -> (bool, bool)
    Or -> (bool, bool)

tyOfUnOp :: UnOp -> (Ty, Ty)
tyOfUnOp op =
  case op of
    Not -> (bool, bool)
    Inv -> (int, int)

tyOfClause :: Ty -> PatClause -> T Ty
tyOfClause tyScrut (PatClause pat bodyE) = do
  (tyPat, bindings) <- tyOfPat pat
  when (tyScrut /= tyPat) $
    failT ("Type of pattern " ++ prettyS pat ++ " is " ++ prettyS tyPat ++
          ", but this does not match type of scrutinee " ++ prettyS tyScrut)
  withNewVars bindings (tyOfExp bodyE)

tyOfPat :: Pat -> T (Ty, VarEnv)
tyOfPat topPat =
  case topPat of
    PatVar x ty -> do
      checkTyOk ty
      pure (ty, M.singleton x ty)
    PatWild ty -> do
      checkTyOk ty
      pure (ty, M.empty)
    PatConstr c tyargs pats -> do
      (tyvars, fieldTys) <- getDecl c
      tySubst <-
        mkTySubst tyvars tyargs ("Type arity mismatch in constructor pattern: " ++ prettyS topPat)
      when (length pats /= length fieldTys) $
        failT ("Arity mismatch in constructor pattern: " ++ prettyS topPat)
      env <- loop (zip pats (map (applyTySubst tySubst) fieldTys))
      pure (TyConstr c tyargs, env)
  where
    loop :: [(Pat, Ty)] -> T VarEnv
    loop [] = pure M.empty
    loop ((p, tyExpected) : rest) = do
      (ty, env) <- tyOfPat p
      when (ty /= tyExpected) $
        failT ("Pattern " ++ prettyS p ++ " should have type " ++ prettyS tyExpected ++
               ", but its type is " ++ prettyS ty)
      restEnv <- loop rest
      let inBoth = M.intersection env restEnv
      when (M.size inBoth /= 0) $
        failT ("Duplicate variables in pattern " ++ prettyS topPat)
      pure (M.union env restEnv)

checkDecl :: Decl -> T ()
checkDecl decl =
  case decl of
    DeclData _ tyvars tys -> do
      when (length tyvars /= S.size (S.fromList tyvars)) $
        failT ("Duplicate type variables in " ++ prettyS decl)
      withNewTyVars tyvars $ forM_ tys checkTyOk
    DeclFun _ ty _ -> do
      checkTyOk ty

checkDeclBody :: Decl -> T ()
checkDeclBody decl =
  case decl of
    DeclData _ _ _ -> pure ()
    DeclFun _ ty body -> checkExp body ty

emptyTyCheckEnv :: TyCheckEnv
emptyTyCheckEnv =
  TyCheckEnv
  { tce_decls = M.empty
  , tce_tyvars = S.empty
  , tce_vars = M.empty
  }

typeCheck :: Prog -> T Ty -- crashes on type errors
typeCheck (Prog decls mainE) = do
  initEnv <- ask
  env <- foldM addToEnv initEnv decls
  local (const env) check
  where
    addToEnv :: TyCheckEnv -> Decl -> T TyCheckEnv
    addToEnv env decl =
      case decl of
        DeclData c tyvars tys ->
          case M.lookup c (tce_decls env) of
            Just _ -> failT ("Duplicate constructor declaration: " ++ prettyS c)
            Nothing -> pure $ env { tce_decls = (M.insert c (tyvars, tys) (tce_decls env)) }
        DeclFun f ty _ ->
          case M.lookup f (tce_vars env) of
            Just _ -> failT ("Duplicate function declaration: " ++ prettyS f)
            Nothing -> pure $ env { tce_vars = (M.insert f ty (tce_vars env)) }
    check :: T Ty
    check = do
      forM_ decls checkDecl
      forM_ decls checkDeclBody
      tyOfExp mainE
