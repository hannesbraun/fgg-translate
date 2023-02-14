{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE OverloadedStrings #-}

module TypeDirectedGeneric.SMLSharpTypedLambda.Language where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.ByteString.Lazy
import qualified Data.Int as I
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics

data Symbol = Symbol String deriving (Generic)
data LongSymbol = LongSymbol [Symbol] deriving (Generic)

data VarID = VarID Int deriving (Generic)
data OPrimID = OPrimID Int deriving (Generic)

data PrimTy = PrimTy {  } -- TODO

data PrimInfo = PrimInfo {  } deriving (Generic) -- TODO
data OPrimInfo = OPrimInfo { oPrimty :: Ty, oPrimPath :: LongSymbol, oPrimId :: OPrimID } deriving (Generic)
data Ty =
    SingletonTy -- TODO
    deriving (Generic)
data VarInfo = VarInfo { varPath :: LongSymbol, varId :: VarID, varTy :: Ty, opaque :: Bool } deriving (Generic)
data ExVarInfo = ExVarInfo { exVarPath :: LongSymbol, exVarTy :: Ty } deriving (Generic)

data TLInt =
    Int8 I.Int8
    | Int16 I.Int16
    | Int32 I.Int32
    | Int64 I.Int64
    deriving (Generic)
-- TODO SML# supports also chars and other stuff

data TLConst =
    Float Float
    | Double Double
    | Unit
    | NullPointer
    | NullBoxed
    | ForeignSymbol { foreignSymbolName :: String, foreignSymbolType :: Ty }
    deriving (Generic)

data TLString =
    String T.Text
    deriving (Generic)
-- TODO IntInf

data Branch = Branch { const :: TLInt, branchBody :: TLExp } deriving (Generic)

data TLExp =
    TLConstant TLConst
    | TLInt TLInt
    | TLString TLString
    | TLVar VarInfo
    | TLExVar ExVarInfo
    | TLFnM { argVarList :: [VarInfo], bodyTy :: Ty, bodyExp :: TLExp }
    | TLAppM { funExp :: TLExp, funTy :: Ty, argExpList :: [TLExp] }
    | TLSwitch { exp :: TLExp, expTy :: Ty, branches :: [Branch], defaultExp :: TLExp, resultTy :: Ty }
    | TLDynamicExistTApp { existInstMap :: TLExp, exp :: TLExp, expTy :: Ty, instTyList :: [Ty] }
    | TLPrimApply { primOp :: PrimInfo, instTyList :: [Ty], argExpList :: [TLExp] }
    | TLOPrimApply { oPrimOp :: OPrimInfo, instTyList :: [Ty], argExp :: TLExp }
    | TLRecord -- TODO
    | TLSelect -- TODO
    | TLModify -- TODO
    | TLLet { decl :: TLDecl, body :: TLExp }
    | TLRaise { exp :: TLExp, resultTy :: Ty }
    | TLHandle { exp :: TLExp, exnVar :: VarInfo, handler :: TLExp, resultTy :: Ty }
    | TLThrow -- TODO
    | TLCatch -- TODO
    | TLPoly -- TODO
    | TLTApp { exp :: TLExp, expTy :: Ty, instTyList :: [Ty] }
    | TLForeignApply -- TODO
    | TLCallbackFn -- TODO
    | TLCast -- TODO
    | TLSizeOf { ty :: Ty }
    | TLIndexOf -- TODO
    | TLReifyTy { ty :: Ty }
    deriving (Generic)

data TLDecl =
    TLVal { var :: VarInfo, declExp :: TLExp }
    | TLValPolyRec -- TODO
    | TLExportVar { weak :: Bool, exVar :: ExVarInfo, declExp :: TLExp }
    | TLExternVar -- TODO
    deriving (Generic)

-- TODO LOC support is missing

data Program = Program { mainExp :: TLExp } deriving (Generic)
-- Not sure what belongs here
-- This is the top level node in a program

instance Aeson.ToJSON PrimInfo
instance Aeson.ToJSON OPrimInfo
instance Aeson.ToJSON OPrimID
instance Aeson.ToJSON Symbol
instance Aeson.ToJSON LongSymbol
instance Aeson.ToJSON VarID
instance Aeson.ToJSON ExVarInfo
instance Aeson.ToJSON VarInfo
instance Aeson.ToJSON Ty
instance Aeson.ToJSON TLString
instance Aeson.ToJSON TLConst
instance Aeson.ToJSON TLInt
instance Aeson.ToJSON Branch
instance Aeson.ToJSON TLExp
instance Aeson.ToJSON TLDecl
instance Aeson.ToJSON Program

serializeToJSON :: Program -> T.Text
serializeToJSON program = decodeUtf8 (toStrict (AesonPretty.encodePretty program))
