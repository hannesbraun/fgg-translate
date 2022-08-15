{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module TypeDirected.TypeDirected where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text

-- import Language.Haskell.Generate
import qualified Common.FGAST as A
import qualified TypeDirected.Target as T
import Common.Utils

-- Nameing conventions

existsName :: Text
existsName = "exists"

lambdaName :: Text
lambdaName = "xX"

unpackName :: Text
unpackName = "yY"

typeName :: Text
typeName = "alpha"

dictName :: Text -> Text -> Text
dictName m instanceType = m <> "_" <> instanceType


-- Auxiliary functions
-------------------------

unIface (A.Iface ms) = ms
unIface x = error ("not an interface: " ++ show x)

distinct xs = nub xs == xs

-- | Fresh names.
freshName :: Text -> State Int Text
freshName s = do
  i <- get
  put (i+1)
  return  (s <> showText i)


isTypeDecl :: A.Decl -> Bool
isTypeDecl (A.Type{}) = True
isTypeDecl _        = False

isIfaceType :: [A.Decl] -> A.TyName -> Bool
isIfaceType decls t =
   case (lookup t $ typeDeclsOf decls) of
     Just (A.Iface{}) -> True
     _                    -> False

isStructType :: [A.Decl] -> A.TyName -> Bool
isStructType decls t =
   case (lookup t $ typeDeclsOf decls) of
     Just (A.Struct{}) -> True
     _                 -> False

isMeDeclWith :: A.TyName -> A.Decl -> Bool
isMeDeclWith t_S (A.Method (_, t_S') _ _) = t_S' == t_S
isMeDeclWith _ _                        = False

toMeSpecUnsafe :: A.Decl -> A.MeSpec
toMeSpecUnsafe (A.Method (_, _) ms _) = ms
toMeSpecUnsafe x = error ("Not a method spec: " ++ show x)

methNames :: [A.MeSpec] -> [A.MeName]
methNames = map A.ms_name

methodsOf :: [A.Decl] -> A.TyName -> [A.MeSpec]
methodsOf ds t_S = map toMeSpecUnsafe $ filter (isMeDeclWith t_S) ds

typeDeclsOf :: [A.Decl] -> [(A.TyName, A.TyLit)]
typeDeclsOf ds = map (\(A.Type n tl) -> (n,tl)) $ filter isTypeDecl ds


-- | xs superset of ys.
-- xs and ys are sorted.
-- We return for each y in ys, the position of the x in xs that equals the y.
isSuper :: Eq a => [a] -> [a] -> Maybe [Int]
isSuper xs ys = isSuper2 (zip [1..] xs) ys

isSuper2 [] []     = Just []
isSuper2 xs []     = Just []
isSuper2 [] (x:xs) = Nothing
isSuper2 ((i,m1):ms1) (m2:ms2)
  | m1 == m2  = do is <- isSuper2 ms1 ms2
                   return (i:is)
  | otherwise = isSuper2 ms1 (m2:ms2)

-- Source to target types
----------------------------

translateMeSig (A.MeSig vts t) =
    T.Func (T.Tuple $ map (\(_,n) -> T.Var (A.unTyName n)) vts) (T.Var (A.unTyName t))

translateTypeLit (A.Struct fts)      = return (T.Tuple $ map (\(_,n) -> T.Var (A.unTyName n)) fts)
translateTypeLit (A.Iface mspecs) = do
      let ts = map (\spec-> translateMeSig (A.ms_sig spec)) mspecs
      n <- freshName existsName
      return (T.Exists n $ T.Tuple $ (T.Var n) : [T.Func (T.Var n) t | t <- ts])
translateTypeLit d@(A.TypeDecl _) = error ("Cannot translate type synonym " ++ show d)

-- | From source to target types.
-- We keep type names of source and only check that they are distinct.
translateTypes :: [A.Decl] -> [(A.TyName, T.Type)]
translateTypes decls = fst $ runState (translateTypes2 decls) 1

translateTypes2 :: [A.Decl] -> State Int [(A.TyName, T.Type)]
translateTypes2 decls = do
  let tyDecls = filter (\x -> case x of
                                A.Type{} -> True
                                _      -> False) decls
  let tyNames = map (\(A.Type n _) -> n) tyDecls
  let check res
        | distinct tyNames = res
        | otherwise        = error "Type names are not distinct"
  types <- mapM translateTypeLit $ map (\(A.Type _ tl) -> tl) tyDecls
  return $ check [(n, T.Mu i (map A.unTyName tyNames) types) | (n,i) <- zip tyNames [1..]]


-- Coercive subtyping
----------------------

lookupUnsafe :: Eq a1 => a1 -> [(a1, a)] -> a
lookupUnsafe x xs = fromJust $ lookup x xs

lookupUnsafeMsg :: Eq a1 => Text -> a1 -> [(a1, a)] -> a
lookupUnsafeMsg msg x xs = case (lookup x xs) of
                             Just y -> y
                             _      -> error (Text.unpack msg)


coerce :: [A.Decl] -> (A.TyName, A.TyName) -> State Int T.Exp
coerce decls (t,u)
   | t == u                                             = do x <- freshName lambdaName
                                                             let phi_t = lookupUnsafe t $ translateTypes decls
                                                             return $ T.Lambda x phi_t $ T.VarExp x
   | isStructType decls t && isIfaceType decls u    = coerceReceiver decls (t,u)
   | isIfaceType decls t && isIfaceType decls u = coerceIface decls (t,u)
   | otherwise = error $ "can't coerce types " ++ show (t,u)

coerceReceiver :: [A.Decl] -> (A.TyName, A.TyName) -> State Int T.Exp
coerceReceiver ds (t_S, t_I) = do
    let tyDecls = typeDeclsOf ds
    -- Sort method names in interfaces
    let iS = sort $ unIface $
               lookupUnsafeMsg "coerceReceiver: interface type not found" t_I tyDecls
    let methodCheck = case (isSuper (sort $ methodsOf ds t_S) iS) of
                        Just _ -> True
                        Nothing -> False
    let phi = translateTypes ds
    let ms = methNames iS
    x <- freshName lambdaName
    let phi_t_S = lookupUnsafe t_S phi
    let delta = lookupUnsafe t_I phi
    return $
      if (not methodCheck)
       then error "coerceReciver: methodCheck fail"
       else T.Lambda x phi_t_S $
              T.Fold delta $
              T.Pack {T.pack =
                        (phi_t_S,
                         T.mkTuple (map T.VarExp $
                                    x : map (\m -> dictName (A.unMeName m) (A.unTyName t_S)) ms)),
                      T.packT = T.expand delta,
                      T.packD = A.unTyName t_I }

coerceIface :: [A.Decl] -> (A.TyName, A.TyName) -> State Int T.Exp
coerceIface ds (t_I, u_I) = do
    let tyDecls = typeDeclsOf ds
    -- Sort methods in interfaces
    let iR = sort $ unIface $
             lookupUnsafeMsg "coerceIface: interface type t not found" t_I tyDecls
    let iS = sort $ unIface $
             lookupUnsafeMsg "coerceIface: interface type u not found" u_I tyDecls
    let msProj = case (isSuper iR iS) of
                   Just is -> is
                   Nothing -> error $ "coerceIface: R not a super set of S" ++ "\n ******" ++ show iR ++ "\n *****" ++ show iS
    let phi = translateTypes ds
    x <- freshName lambdaName
    y <- freshName unpackName
    alpha <- freshName typeName
    let gamma = lookupUnsafe t_I phi
    let delta = lookupUnsafe u_I phi
    let n = 1 + length iR
    return $ T.Lambda x gamma $
              T.Unpack {
                T.unpackD = A.unTyName t_I,
                T.unpack   = (alpha, y),
                T.unpackE1 = T.Unfold gamma $ T.VarExp x,
                T.unpackE2 =  T.Fold delta $
                              T.Pack {
                              T.pack = (T.Var alpha,
                                        T.mkTuple $ (T.mkProj (1,n) (T.VarExp y))
                                                    : (map (\i -> T.mkProj (i+1,n) (T.VarExp y)) msProj)),
                              T.packT = T.expand delta,
                              T.packD = A.unTyName u_I }}


-- Type-directed translation
------------------------------

errorExp s = error $ "translateExp: " ++ s

inferTranslateExp :: [A.Decl] -> [(A.VarName, A.TyName)] -> A.Exp -> State Int (A.TyName, T.Exp)
inferTranslateExp _ env (A.Var x) =
  case (lookup x env) of
    Just t  -> return $ (t, T.VarExp (A.unVarName x))
    Nothing -> errorExp $ "unkonwn variable " ++ show x
inferTranslateExp decls env (A.StructLit t es) =
  case (lookup t $ typeDeclsOf decls) of
     Just (A.Struct fs) ->
        if length es /= length fs
          then errorExp $ "number of field elements is wrong"
          else do
            let ts = map snd fs
            uEs <- mapM (inferTranslateExp decls env) es
            let  us = map fst uEs
                 eEs = map snd uEs
            cs <- mapM (coerce decls) (zip us ts)
            return (t, T.TupleExp { T.tupleName = Just t,
                                    T.tupleExps = map (\(c,e) -> T.App c e) (zip cs eEs)})
     _ -> errorExp $ "unknown structure " ++ show t

inferTranslateExp decls env (A.Select e f) =
  do (t,eE) <- inferTranslateExp decls env e
     case (lookup t $ typeDeclsOf decls) of
       Just (A.Struct fs) ->
         case (find (\(_,(f',_)) -> f == f') (zip [1..] fs)) of
            Just (j, (fj, tj)) ->
                     return (t,
                             T.Proj { T.projName = Just t,
                                      T.projIdxs = (1, length fs),
                                      T.projExps = eE })
            Nothing -> errorExp $ "field unknown " ++ show (t,f)
       _            -> errorExp $ "type not a structure " ++ show t

inferTranslateExp decls env (A.MeCall e m es) =
  do (t_I, eE) <- inferTranslateExp decls env e
     case (lookup t_I (typeDeclsOf decls)) of
       Just (A.Iface methSpecs) ->
         case (find (\(_, spec) -> m == A.ms_name spec) (zip [1..] (sort methSpecs))) of
           Just (j, (A.MeSpec _ (A.MeSig args u))) -> do
              let n = length methSpecs -- j specific position within methSpecs
                                       -- Haskell backend needs j and n
              let ts = map snd args
              if length ts /= length es
                then errorExp $ "number method call arguments doesn't match up with specification"
                else do
                  uEs <- mapM (inferTranslateExp decls env) es
                  let us = map fst uEs
                      eEs = map snd uEs
                      phi = translateTypes decls
                      delta = lookupUnsafe t_I phi
                  cs <- mapM (coerce decls) (zip us ts)
                  alpha <- freshName typeName
                  x <- freshName unpackName
                  return (u,
                          T.Unpack {
                             T.unpackD = A.unTyName t_I,
                             T.unpack  = (alpha, x),
                             T.unpackE1 = T.Unfold delta eE,
                             T.unpackE2 = T.App (T.App (T.mkProj (j,n) (T.VarExp x)) (T.mkProj (1,n) (T.VarExp x)))
                                                (T.mkTuple $ map (\(c,e)-> T.App c e) (zip cs eEs))
                          }
                         )
           Nothing                          -> errorExp $ "unknown method call " ++ show m
       Just _                  -> errorExp $ "not an interface " ++ show t_I
       Nothing                 -> errorExp $ "unknown type " ++ show t_I

inferTranslateExp _ _ e = error ("Cannot translate expression " ++ show e)

run x = fst $ runState x 1

translateProg :: ([A.Decl], A.Exp) -> State Int ([(A.TyName, T.Type)], A.TyName, T.Exp)
translateProg (decls, e) = do
    (t,eE) <- inferTranslateExp decls [] e
    return (translateTypes decls, t, eE)

translate p = run $ translateProg p
