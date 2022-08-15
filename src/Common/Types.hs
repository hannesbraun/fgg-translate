{-# LANGUAGE DeriveDataTypeable #-}
module Common.Types where

import Data.Data

data WithCasts = WithCasts | WithoutCasts
               deriving (Eq, Ord, Show, Data, Typeable)

data BinOp = Plus | Minus | Mult | Div | Mod
           | Equal | NotEqual
           | Lt | LtEqual | Gt | GtEqual
           | And | Or
             deriving (Eq, Ord, Show, Data, Typeable)

data UnOp = Not
             deriving (Eq, Ord, Show, Data, Typeable)

data TraceFlag = TraceOn | TraceOff
  deriving (Eq, Show)
