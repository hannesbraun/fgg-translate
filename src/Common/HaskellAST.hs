{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.HaskellAST where

import Common.Types
import Common.Utils

import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import Safe

data Prog = Prog
  { p_decls :: [Decl]
  , p_main :: Main
  }
  deriving (Eq, Show)

-- All expressions in Main are forced
data Main = Main
  { m_bindings :: [(VarName, Exp)]
  , m_result :: Exp
  }
  deriving (Eq, Show)

data Decl
  = DeclClass ClassDecl
  | DeclInst InstDecl
  | DeclData DataDecl
  | DeclNewtype NewtypeDecl
  | DeclVal ValDecl
  deriving (Eq, Show)

--
-- Classes
--
data ClassDecl = ClassDecl
  { cd_tyvars :: [TyVarName]
  , cd_context :: [Constr]
  , cd_className :: ClassName
  , cd_funDeps :: [FunDep]
  , cd_methodDecls :: [MeDecl]
  }
  deriving (Eq, Show)

data MeDecl = MeDecl
  { mdec_name :: VarName
  , mdec_sig :: Sigma
  }
  deriving (Eq, Show)

data FunDep = FunDep
  { fd_lhs :: [TyVarName]
  , fd_rhs :: TyVarName
  }
  deriving (Eq, Show)

--
-- Instances
--
data InstDecl = InstDecl
  { id_flags :: InstFlags
  , id_tyvars :: Set TyVarName
  , id_context :: [Constr]
  , id_className :: ClassName
  , id_types :: [Tau]
  , id_methodDefs :: [MethodDef]
  }
  deriving (Eq, Show)

newtype InstFlags = InstFlags {unInstFlags :: Set InstFlag}
  deriving (Eq, Show)

noInstFlags :: InstFlags
noInstFlags = InstFlags Set.empty

overlappableFlags :: InstFlags
overlappableFlags = InstFlags (Set.singleton InstOverlappable)

data InstFlag = InstOverlappable
  deriving (Eq, Ord, Show)

data MethodDef = MethodDef
  { mdef_name :: VarName
  , mdef_exp :: Exp
  }
  deriving (Eq, Show)

--
-- Data types
--

data DataDecl = DataDecl
  { dd_name :: TyConName
  , dd_tyVars :: [TyVarName]
  , dd_alts :: [Constructor]
  , dd_derive :: [ClassName]
  }
  deriving (Eq, Show)

data Constructor = Constructor
  { ctor_name :: DataConName
  , ctor_forall :: [TyVarName]
  , ctor_context :: [Constr]
  , ctor_args :: [Tau]
  , ctor_result :: Maybe Tau -- Nothing for non-GADTs
  }
  deriving (Eq, Show)

simpleCtor :: DataConName -> Constructor
simpleCtor k = Constructor k [] [] [] Nothing

--
-- Newtype decls
--

data NewtypeDecl = NewtypeDecl
  { nt_name :: TyConName
  , nt_constr :: DataConName
  , nt_unConstr :: VarName
  , nt_wrappedType :: TyConName
  }
  deriving (Eq, Show)

--
-- Value decls
--

data ValDecl = ValDecl
  { vd_sig :: Maybe T.Text
  , vd_name :: VarName
  , vd_args :: [VarName]
  , vc_exp :: Exp
  }
  deriving (Eq, Show)

--
-- Constraints
--

-- pi
data Constr = Constr
  { c_tyVars :: Set TyVarName
  , c_lhs :: [ClassConstr]
  , c_rhs :: ClassConstr
  }
  deriving (Eq, Show)

simpleConstr :: ClassConstr -> Constr
simpleConstr cc = Constr Set.empty [] cc

-- chi
data ClassConstr = ClassConstr
  { cc_className :: ClassName
  , cc_types :: [Tau]
  }
  deriving (Eq, Show)

--
-- Types
--

data Tau
  = TypeTyVar TyVarName
  | TypeArrow Tau Tau
  | TypeTyCon TyConName [Tau]
  deriving (Eq, Show)

simpleTyCon :: TyConName -> Tau
simpleTyCon n = TypeTyCon n []

tv :: TyVarName -> Tau
tv = TypeTyVar

infixr 4 `to`
to :: Tau -> Tau -> Tau
to = TypeArrow

funType :: [Tau] -> Tau -> Tau
funType args res = foldr to res args

tupleType :: [Tau] -> Tau
tupleType ts = TypeTyCon (tupleTyCon (length ts)) ts

data Sigma = Sigma
  { s_tyVars :: Set TyVarName
  , s_context :: [Constr]
  , s_tau :: Tau
  }
  deriving (Eq, Show)

simpleSigma :: Tau -> Sigma
simpleSigma = Sigma Set.empty []

--
-- Exps
--

data Exp
  = ExpVar VarName
  | ExpDataCon DataConName
  | ExpApp Exp Exp
  | ExpLambda VarName Exp
  | ExpLet [(VarName, Exp)] Exp
  | ExpCase Exp [(Pat, Exp)]
  | ExpBinOp BinOp Exp Exp
  | ExpUnOp UnOp Exp
  | ExpIte Exp Exp Exp
  | ExpIntLit Integer
  | ExpCharLit Char
  | ExpStrLit T.Text
  deriving (Eq, Show)

data Pat = Pat
  { p_dataCon :: DataConName
  , p_vars :: [VarName]
  }
  deriving (Eq, Show)

infixl 4 `app`
app :: Exp -> Exp -> Exp
app = ExpApp

var :: VarName -> Exp
var = ExpVar

tupleExp :: [Exp] -> Exp
tupleExp es =
  let e0 = ExpDataCon (tupleDataCon (length es))
   in foldl ExpApp e0 es

--
-- Names
--
newtype ClassName = ClassName {unClassName :: T.Text}
  deriving (Eq, Ord, IsString, Show)

newtype TyVarName = TyVarName {unTyVar :: T.Text}
  deriving (Eq, Ord, IsString, Show)

-- reserved TyConName: '()N' for pairs of arity N
newtype TyConName = TyConName {unTyConName :: T.Text}
  deriving (Eq, Ord, IsString, Show)

tupleTyCon :: Int -> TyConName
tupleTyCon n = TyConName ("()" <> showText n)

isTuple :: T.Text -> Maybe Int
isTuple t =
  if "()" `T.isPrefixOf` t
    then readMay (drop 2 (T.unpack t))
    else Nothing

isTupleTyCon :: TyConName -> Maybe Int
isTupleTyCon (TyConName n) = isTuple n

-- reserved DataConName: '()N' for pairs of arity N
newtype DataConName = DataConName {unDataConName :: T.Text}
  deriving (Eq, Ord, IsString, Show)

tupleDataCon :: Int -> DataConName
tupleDataCon n = DataConName ("()" <> showText n)

isTupleDataCon :: DataConName -> Maybe Int
isTupleDataCon (DataConName n) = isTuple n

newtype VarName = VarName {unVar :: T.Text}
  deriving (Eq, Ord, IsString, Show)

--
-- Free type variables
--
class FreeTyVars a where
  freeTyVars :: a -> Set TyVarName

instance FreeTyVars Tau where
  freeTyVars tau =
    case tau of
      TypeTyVar a -> Set.singleton a
      TypeArrow t1 t2 -> freeTyVars t1 `Set.union` freeTyVars t2
      TypeTyCon _ taus -> freeTyVars taus

instance FreeTyVars a => FreeTyVars [a] where
  freeTyVars l = Set.unions (map freeTyVars l)
