{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.SystemF.Typechecker (

  typeCheck, erase, htf_thisModulesTests

  ) where

import Common.Types
import Common.PrettyUtils
import Prettyprinter
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL

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
import qualified Data.List as List
import Data.Maybe
import Data.Data hiding (Constr)
import qualified Data.Text as T
import Data.String
import Data.Generics
import Test.Framework

import TypeDirectedGeneric.SystemF.Syntax
import TypeDirectedGeneric.SystemF.Pretty

newtype TySubst = TySubst { unTySubst :: Map TyVarName Ty }

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

withNewTyVar :: TyVarName -> T a -> T a
withNewTyVar a comp =
  local (\env -> env { tce_tyvars = S.insert a (tce_tyvars env) }) comp

newtype T a = T { unT :: RWST TyCheckEnv () () (ExceptT TyCheckError TyCheckTracer) a }
    deriving ( Functor, Applicative, Monad, MonadReader TyCheckEnv, MonadState ()
             , MonadError String)

failT :: String -> T a
failT = throwError

checkTyOk :: Ty -> T ()
checkTyOk = undefined

checkExp :: Exp -> Ty -> T ()
checkExp = undefined

tyOfExp :: Exp -> T Ty
tyOfExp e = do
  venv <- readVars
  tenv <- readTyvars
  case e of
    ExpVar x -> do
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
          failT ("Left-hand side of function application is not a function type: " ++ prettyS ty1)
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
    ExpCase e clauses -> undefined
    ExpBinOp e1 op e2 -> undefined
    ExpUnOp op e -> undefined
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


typeCheck :: Prog -> IO () -- crashes on type errors
typeCheck _ = pure () -- FIXME
-- Decls:
-- data: check tyvars unique

erase :: Prog -> TL.Prog
erase = undefined -- FIXME
