{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common.FGAST where

import Common.Types

import Data.String
import qualified Data.Text as T

newtype FieldName = FieldName { unFieldName :: T.Text }
    deriving (Eq, Ord, Show, IsString)

newtype MeName = MeName { unMeName :: T.Text }
    deriving (Eq, Ord, Show, IsString)

newtype VarName = VarName { unVarName :: T.Text }
    deriving (Eq, Ord, Show, IsString)

newtype TyName = TyName { unTyName :: T.Text }
    deriving (Eq, Ord, Show, IsString)

data MeSig
    = MeSig
      { msig_args :: [(VarName, TyName)]
      , msig_res :: TyName
      }
    deriving (Show)

data MeSpec
    = MeSpec
      { ms_name :: MeName
      , ms_sig :: MeSig
      }
    deriving (Eq, Ord, Show)

ms_result :: MeSpec -> TyName
ms_result = msig_res . ms_sig

data TyLit = Struct [(FieldName, TyName)]
                 | TypeDecl TyName
                 | Iface [MeSpec]
                  deriving (Eq, Show)

data Decl = Type TyName TyLit
                 | Method (VarName, TyName) MeSpec Exp
                  deriving (Eq, Show)

data Exp = Var VarName
                | MeCall Exp MeName [Exp]
                | StructLit TyName [Exp]
                | Select Exp FieldName
                | TyAssert Exp TyName
                | BinOp BinOp Exp Exp
                | UnOp UnOp Exp
                | Cond Exp Exp Exp
                | IntLit Integer
                | StrLit T.Text
                | CharLit Char
                  deriving (Eq, Show)

data Program
    = Program
    { p_decls :: [Decl]
    , p_main :: Main
    }
    deriving (Eq, Show)

data Main
    = Main
    { m_bindings :: [(VarName, Exp)]
    , m_result :: Exp
    }
    deriving (Eq, Show)

canonMethodSig :: MeSig -> ([TyName], TyName)
canonMethodSig (MeSig args res) = (map snd args, res)

-- For equality checking, we only consider types but not variable names of parameters.
instance Eq MeSig where
    (==) sig1 sig2 = canonMethodSig sig1 == canonMethodSig sig2

instance Ord MeSig where
    compare sig1 sig2 = compare (canonMethodSig sig1) (canonMethodSig sig2)
