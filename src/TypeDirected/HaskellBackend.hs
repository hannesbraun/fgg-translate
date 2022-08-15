{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module TypeDirected.HaskellBackend where

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Common.FGAST as A
import qualified TypeDirected.Target as T
import qualified TypeDirected.TypeDirected as TD


-- maybe later, seems almost overkill for our current target
-- import Language.Haskell.Exts
-- import Language.Haskell.Exts.Syntax


-- quick and dirty

prefixData = "H_"
prefixTyVar = "a_"

prefixDataUnMk = "un"

-- | Translate tuples to pairs nested to the right
translateTuples [] = "()"
translateTuples [x] = "(" <> x <> ")"
translateTuples (x:y:ys) = "(" <> x <> "," <> translateTuples (y:ys) <> ")"

-- vs name of the type names that give rise to data type equations.
-- v the current (data type) name we consider.
toHaskellTypesInnerLevel :: [A.TyName] -> T.Type -> Text
toHaskellTypesInnerLevel vs (T.Var n)
  | elem (A.TyName n) vs = prefixData <> n
  | otherwise = prefixTyVar <> n
toHaskellTypesInnerLevel vs (T.Tuple ts) = translateTuples $ map (toHaskellTypesInnerLevel vs) ts
toHaskellTypesInnerLevel vs (T.Func t1 t2) = "(" <> toHaskellTypesInnerLevel vs t1 <> " -> " <> toHaskellTypesInnerLevel vs t2 <> ")"
toHaskellTypesInnerLevel _ t = error ("toHaskellTypesInnerLevel: " ++ show t)


-- For each structured data type (tuple) generate a deconstructor as well (prefixDataUnMk)
toHaskellTypesOuterLevel :: A.TyName -> [A.TyName] -> T.Type -> Text
toHaskellTypesOuterLevel v vs (T.Tuple ts) =
    "data " <> prefixData <> A.unTyName v <> " = " <> prefixData <> A.unTyName v <> " " <>
    (translateTuples $ map (toHaskellTypesInnerLevel vs) ts)
    <> "\n" <>
    prefixDataUnMk <> A.unTyName v <> " (" <> prefixData <> A.unTyName v <> " x) = x"
toHaskellTypesOuterLevel v vs (T.Exists n t) =
    "data " <> prefixData <> A.unTyName v
    <> " = forall " <> prefixTyVar <> n <> ". "
    <> prefixData <> A.unTyName v <> " "
    <> toHaskellTypesInnerLevel vs t
toHaskellTypesOuterLevel _ _ t = error ("toHaskellTypesOuterLevel: " ++ show t)

projHaskell :: (Int, Int) -> Text
projHaskell (i,n)
  | i == n && i == 1 = "\\ x -> x"
  | i < n  && i == 1 = "fst"
  | i == n && i > 1  = Text.concat $ intersperse "." $ take (n-1) $ repeat " snd"
  | i < n  && i > 1  = "fst ." <> (Text.concat $ intersperse "." $ take (i-1) $ repeat " snd")
  | otherwise        = error "proj"


toHaskellTypes :: [(A.TyName, T.Type)] -> Text
toHaskellTypes xs =
    let vs = map fst xs
    in Text.unlines $ map (\(v, T.Mu i _ types) -> toHaskellTypesOuterLevel v vs (types !! (i-1))) xs


translateTypes :: [A.Decl] -> Text
translateTypes = toHaskellTypes . TD.translateTypes

-- | Translate to Haskell.
-- Represent tuples as lists.
-- Ignore Fold/Unfold cause always coupled with pack/unpack.
-- Translate pack/unpack to data type construction/deconstrution.
-- Target terms are annotated with the names of data types + constructor.
translateExp :: T.Exp -> Text
translateExp (T.VarExp x)
   = x
translateExp (tuple@T.TupleExp{})
  = case (T.tupleName tuple) of
      Nothing  -> translateTuples $ map translateExp (T.tupleExps tuple)
      Just t_S -> prefixData <> A.unTyName t_S <> " " <> (translateTuples $ map translateExp (T.tupleExps tuple))
translateExp (proj@T.Proj{})
  = case (T.projName proj) of
      Nothing  -> "(" <> projHaskell (T.projIdxs proj) <> ") " <> (translateExp $ T.projExps proj)
      Just t_S -> "(" <> projHaskell (T.projIdxs proj) <> ") " <>
                  "(" <> prefixDataUnMk <> A.unTyName t_S <> " " <> (translateExp $ T.projExps proj) <> ")"
translateExp (T.App e1 e2)
  = "(" <> translateExp e1 <> " " <> translateExp e2 <> ")"
translateExp (T.Lambda v _ e)
  = "(" <> "\\ " <> v <> " -> " <> translateExp e <> ")"
translateExp (e@T.Pack{})
  = prefixData <> T.packD e <> " $ " <> translateExp (snd (T.pack e))
translateExp (e@T.Unpack{})
  = "case " <> translateExp (T.unpackE1 e) <> " of; "
    <> prefixData <> T.unpackD e <> " " <> snd (T.unpack e) <> " -> " <> translateExp (T.unpackE2 e)
translateExp (T.Letrec bs e)
  = "let " <> (Text.concat $ map (\(v,_,e) -> v <> " = " <> translateExp e <> ";") bs) <> "in " <> translateExp e
translateExp (T.Fold _ e)
  = translateExp e
translateExp (T.Unfold _ e)
  = translateExp e





{-
f x = let y = x+1; in y

g x = case x of; Just y -> y + 1



h = let g x = f x; f x = g x; in (f,f)

-}
