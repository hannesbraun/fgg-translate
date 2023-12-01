{-# LANGUAGE OverloadedStrings #-}

module Common.PrettyUtils where

import Common.Types
import qualified Data.List as List
import qualified Data.Text as T
import Prettyprinter

text :: T.Text -> Doc a
text = pretty

vcatBy :: Doc a -> [Doc a] -> Doc a
vcatBy sep l =
  vcat $ List.intersperse sep l

sepBy :: Doc a -> [Doc a] -> Doc a
sepBy sep l = hcat $ List.intersperse sep l

termBy :: Doc a -> [Doc a] -> Doc a
termBy _ [] = mempty
termBy t l = sepBy t l <> t

type Prec = Int

maxPrec :: Prec
maxPrec = 10

withParens :: Prec -> Prec -> Doc a -> Doc a
withParens outer inner d =
  if outer >= inner then parens d else d

class PrettyPrec a where
  -- precendence is the precendence of the surrounding context (as for showsPrec)
  prettyPrec :: Prec -> a -> Doc b

renderList :: Pretty a => T.Text -> T.Text -> [a] -> T.Text -> Doc ann
renderList left sep l right =
  case l of
    [] -> emptyDoc
    _ -> text left <> sepBy (text sep) (map pretty l) <> text right <> emptyDoc

prettyS :: Pretty a => a -> String
prettyS = show . pretty

prettyT :: Pretty a => a -> T.Text
prettyT = T.pack . show . pretty

precBinOp :: BinOp -> Int
precBinOp op =
  case op of
    Plus -> 6
    Minus -> 6
    Mult -> 7
    Div -> 7
    Mod -> 7
    Equal -> 4
    NotEqual -> 4
    Lt -> 3
    LtEqual -> 3
    Gt -> 3
    GtEqual -> 3
    And -> 2
    Or -> 1

newtype CommaList a = CommaList [a]

instance Pretty a => Pretty (CommaList a) where
  pretty (CommaList l) = prettyCommas l

prettyCommas :: Pretty a => [a] -> Doc b
prettyCommas l = sepBy (text ", ") (map pretty l)
