{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module TypeDirectedGeneric.SExp (

    SExp(..), SExps(..)

) where

import Common.Utils

import Data.Data
import Prettyprinter
import qualified Data.Text as T

data SExp
    = SExpInt Integer
    | SExpBool Bool
    | SExpStr T.Text
    | SExpChar Char
    | SExpSym T.Text
    | SExpVar T.Text
    | SExp [SExp]
    deriving (Eq, Ord, Show, Data, Typeable)

text :: T.Text -> Doc a
text = pretty

newtype SExps = SExps { unSExps :: [SExp] }
    deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty SExp where
    pretty sexp =
        case sexp of
          SExpInt i -> text (showText i)
          SExpBool True -> text "#t"
          SExpBool False -> text "#f"
          SExpStr t -> text (showText t)
          SExpChar c -> text ("#\\" <> showText c)
          SExpSym sym -> text ("'" <> sym)
          SExpVar var -> text var
          SExp es -> parens (align (sep (map pretty es)))

instance Pretty SExps where
    pretty (SExps sexps) = vsep (map pretty sexps)
