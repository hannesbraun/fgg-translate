{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Common.FGGAST where

import Common.Types
import Common.Utils

import Test.Framework
import Data.Data
import Data.String
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

newtype FieldName = FieldName { unFieldName :: T.Text }
    deriving (Eq, Ord, Show, IsString, Data, Typeable)

newtype MeName = MeName { unMeName :: T.Text }
    deriving (Eq, Ord, Show, IsString, Data, Typeable)

newtype VarName = VarName { unVarName :: T.Text }
    deriving (Eq, Ord, Show, IsString, Data, Typeable)

newtype TyVarName = TyVarName { unTyVarName :: T.Text }
    deriving (Eq, Ord, Show, IsString, Data, Typeable)

newtype TyName = TyName { unTyName :: T.Text }
    deriving (Eq, Ord, Show, IsString, Data, Typeable)

newtype TyFormals = TyFormals { unTyFormals :: [(TyVarName, Maybe Type)] }
    deriving (Show, Eq, Data, Typeable, Semigroup)

data Type
    = TyVar TyVarName
    | TyNamed TyName [Type]
    deriving (Show, Eq, Ord, Data, Typeable)

tyVoid :: Type
tyVoid = TyNamed (TyName "void") []

isVoid :: Type -> Bool
isVoid (TyNamed (TyName "void") []) = True
isVoid _ = False

data MeSig
    = MeSig
      { msig_tyArgs :: TyFormals
      , msig_args :: [(VarName, Type)]
      , msig_res :: Type
      }
    deriving (Show, Data, Typeable)

data MeSpec
    = MeSpec
      { ms_name :: MeName
      , ms_sig :: MeSig
      }
    deriving (Eq, Ord, Show, Data, Typeable)

ms_result :: MeSpec -> Type
ms_result = msig_res . ms_sig

data TyLit = Struct [(FieldName, Type)]
           | TySyn Type
           | Iface [MeSpec]
                  deriving (Eq, Show, Data, Typeable)

data MeBody
    = MeBody
    { mb_bindings :: [(VarName, Maybe Type, Exp)]
    , mb_return :: Maybe Exp
    }
    deriving (Eq, Show, Data, Typeable)

data Decl = TypeDecl TyName TyFormals TyLit
          | MeDecl (VarName, TyName, TyFormals) MeSpec MeBody
          | FunDecl MeSpec MeBody
                  deriving (Eq, Show, Data, Typeable)

data Exp = Var VarName
         | MeCall Exp MeName [Type] [Exp]
         | FunCall MeName [Type] [Exp]
         | StructLit Type [Exp]
         | Select Exp FieldName
         | TyAssert Exp Type
         | BinOp BinOp Exp Exp
         | UnOp UnOp Exp
         | Cond Exp Exp Exp
         | BoolLit Bool
         | IntLit Integer
         | StrLit T.Text
         | CharLit Char
           deriving (Eq, Show, Data, Typeable)

data Program
    = Program
    { p_decls :: [Decl]
    , p_mains :: [MeBody]
    }
    deriving (Eq, Show, Data, Typeable)

canonMethodSig :: MeSig -> ([Maybe Type], [Type], Type)
canonMethodSig (MeSig (TyFormals tyArgs) args resType) =
    let boundTypes = map snd tyArgs
        argTypes = map snd args
        s = Map.fromList $
            zipWith (curry (\((a, _), i) -> (a, TyVar (TyVarName ("__" <> showText i))))) tyArgs [0..]
    in (applyTySubst s boundTypes, applyTySubst s argTypes, applyTySubst s resType)

-- For equality checking, we only consider types but not variable names of parameters.
instance Eq MeSig where
    (==) sig1 sig2 = canonMethodSig sig1 == canonMethodSig sig2

instance Ord MeSig where
    compare sig1 sig2 = compare (canonMethodSig sig1) (canonMethodSig sig2)

class FreeTyVars a where
    freeTyVars :: a -> Set TyVarName

instance FreeTyVars Type where
    freeTyVars t =
        case t of
          TyVar tyVar -> Set.singleton tyVar
          TyNamed _ args ->
              Set.unions (map freeTyVars args)

instance FreeTyVars a => FreeTyVars [a] where
    freeTyVars l = Set.unions (map freeTyVars l)

instance FreeTyVars MeSig where
    freeTyVars sig =
        let boundVars = Set.fromList $ map fst (unTyFormals (msig_tyArgs sig))
            allTypes =
                mapMaybe snd (unTyFormals (msig_tyArgs sig)) ++
                map snd (msig_args sig) ++
                [msig_res sig]
        in freeTyVars allTypes `Set.difference` boundVars

type TySubst = Map TyVarName Type

class TySubstable a where
    applyTySubst :: TySubst -> a -> a

instance TySubstable Type where
    applyTySubst s t =
        case t of
          TyVar tyVar -> Map.findWithDefault t tyVar s
          TyNamed name args ->
              TyNamed name (applyTySubst s args)

instance TySubstable a => TySubstable [a] where
    applyTySubst s = map (applyTySubst s)

instance TySubstable a => TySubstable (Maybe a) where
    applyTySubst s = fmap (applyTySubst s)

instance TySubstable MeSpec where
    applyTySubst s spec = spec { ms_sig = applyTySubst s (ms_sig spec) }

instance TySubstable MeSig where
    applyTySubst subst sig =
        let -- Limit the domain of the substitution to variables that
            -- occur free in sig.
            -- This avoids substituting bound variables and minimizes the number
            -- of bound variables that we need to rename
            freeInSig = freeTyVars sig
            limitedSubst = Map.filterWithKey (\v _ -> v `Set.member` freeInSig) subst
            -- do not capture bound variables
            boundVars = Set.fromList $ map fst (unTyFormals (msig_tyArgs sig))
            freeInSubst = freeTyVars (Map.elems limitedSubst)
            needToRename = boundVars `Set.intersection` freeInSubst
            renaming =
                Map.fromList $
                map (findFresh (freeInSubst `Set.union` boundVars) 0)
                    (Set.toList needToRename)
            renamingSubst = Map.map TyVar renaming
            applySubsts t = applyTySubst limitedSubst (applyTySubst renamingSubst t)
        in
          MeSig
          { msig_tyArgs =
                TyFormals $ flip map (unTyFormals (msig_tyArgs sig)) $ \(a, t) ->
                let a' = Map.findWithDefault a a renaming
                    t' = applySubsts t
                in (a', t')
          , msig_args = map (\(x, t) -> (x, applySubsts t)) (msig_args sig)
          , msig_res = applySubsts (msig_res sig)
          }
        where
          findFresh :: Set TyVarName -> Int -> TyVarName -> (TyVarName, TyVarName)
          findFresh forbidden i (TyVarName var) =
              let cand = TyVarName (var <> "$" <> showText i)
              in if cand `Set.member` forbidden
                 then findFresh forbidden (i+1) (TyVarName var)
                 else (TyVarName var, cand)

test_tySubstMsig :: IO ()
test_tySubstMsig = do
  let subst = Map.fromList [ ("a", TyNamed "Box" [TyVar "b"]) -- b would be captured
                           , ("c", TyNamed "Int" []) -- c is bound by sig
                           , ("e", TyVar "d") -- d would be captured but e is not free in sig
                           ]
      sig =
          MeSig
          { msig_tyArgs =
                TyFormals [ ("c", Just (TyNamed "EQ" [TyVar "c"]))
                          , ("b", Nothing)
                          , ("d", Just (TyNamed "I" [TyVar "a"]))
                          ]
          , msig_args = [ ("x", TyNamed "Box" [TyVar "b"])
                        , ("y", TyNamed "Pair" [TyVar "a", TyVar "c"])]
          , msig_res = TyNamed "Pair" [TyVar "d", TyVar "f"]
          }
      substSig =
          MeSig
          { msig_tyArgs =
                TyFormals [ ("c", Just (TyNamed "EQ" [TyVar "c"]))
                          , ("b$0", Nothing)
                          , ("d", Just (TyNamed "I" [TyNamed "Box" [TyVar "b"]]))
                          ]
          , msig_args = [ ("x", TyNamed "Box" [TyVar "b$0"])
                        , ("y", TyNamed "Pair" [TyNamed "Box" [TyVar "b"], TyVar "c"])]
          , msig_res = TyNamed "Pair" [TyVar "d", TyVar "f"]
          }
  assertEqual substSig (applyTySubst subst sig)
