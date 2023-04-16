{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeDirectedGeneric.SystemF.Tests (htf_thisModulesTests) where

import TypeDirectedGeneric.SystemF.Parser
import TypeDirectedGeneric.SystemF.Syntax
import TypeDirectedGeneric.SystemF.Erasure
import TypeDirectedGeneric.SystemF.Typechecker
import qualified TypeDirectedGeneric.UntypedTargetLanguage as TL
import Common.Utils

import Test.Framework
import Text.RawString.QQ
import qualified Data.Text as T

parseOrDie :: FParser p -> T.Text -> IO p
parseOrDie parser src =
  case runP parser src of
    Right p -> pure p
    Left err -> do
      putStrLn (T.unpack src)
      fail (T.unpack err)

run :: Prog -> IO T.Text
run p = do
  let tl = erase p
  res <- TL.evalProg "/tmp/fgg-target.rkt" "" tl
  case res of
    Left err -> fail ("Evaluating System F program failed: " ++ T.unpack err)
    Right x -> pure x

check :: T.Text -> T.Text -> T.Text -> IO ()
check src tySrc res = do
  ty <- parseOrDie parseTy tySrc
  prog <- parseOrDie parseProg src
  givenTy <- eitherFail $ runT (typeCheck prog)
  assertEqual ty givenTy
  givenRes <- run prog
  assertEqual res givenRes

checkIlltyped :: T.Text -> IO ()
checkIlltyped src = do
  prog <- parseOrDie parseProg src
  case runT (typeCheck prog) of
    Left _ -> pure ()
    Right x -> assertFailure ("Program type checked successfully with type " ++ show x ++
                              " but should fail")

test_fac :: IO ()
test_fac = do
  check prog "Int" "24"
  checkIlltyped progWrong
  where
    prog = [r|
fun fac : Int -> Int = \i:Int . if i == 0 then 1 else i * fac (i - 1)

let main = fac 4
|]
    progWrong = [r|
fun fac : Int -> Bool = \i:Int . if i == 0 then 1 else i * fac (i - 1)

let main = fac 4
|]

test_pairs :: IO ()
test_pairs =
  check prog "Int" "1"
  where
    prog = [r|
data Pair a b = a b
data NestedPair a b = (Pair a b) (Pair a b)

fun mkPair : forall a b . a -> b -> NestedPair a b =
  \\a . \\b . \x:a . \y:b . NestedPair @a b { Pair @a b {x, y}, Pair @a b {x, y} }

fun getSome : forall a . NestedPair Int a -> Int =
  \\a . \p: NestedPair Int a .
  case p of NestedPair @Int a { Pair @Int a { x: Int, _: a }, Pair @Int a { _: Int, _: a } } -> x

let main = getSome @Bool (mkPair @Int @Bool 1 True)
|]

test_poly :: IO ()
test_poly = do
  check prog1 "Bool" "#t"
  checkIlltyped prog2
  where
    prog1 = [r|
fun poly : forall a . a -> a = \\a . \x:a . x

let main = poly @Bool True
|]
    prog2 = [r|
fun poly : forall a . a = \\a . \x:a . x

let main = poly True
|]

test_parseTy :: IO ()
test_parseTy = do
  subAssert $ assertParseTy "a -> b" (TyArrow "a" "b")
  subAssert $ assertParseTy "forall a . a" (TyForall "a" (TyVar "a"))
  subAssert $ assertParseTy "Int" (TyPrim PrimInt)
  subAssert $ assertParseTy "K" (TyConstr "K" [])
  subAssert $ assertParseTy "K Int" (TyConstr "K" [TyPrim PrimInt])
  subAssert $ assertParseTy "K Int (L Bool)"
    (TyConstr "K" [TyPrim PrimInt, TyConstr "L" [TyPrim PrimBool]])
  subAssert $ assertParseTy "a -> b" (TyArrow "a" "b")
  subAssert $ assertParseTy "Int -> Bool" (TyArrow (TyPrim PrimInt) (TyPrim PrimBool))
  subAssert $ assertParseTy "forall a . a -> b" (TyForall "a" (TyArrow "a" "b"))
  subAssert $ assertParseTy "a -> b -> c" (TyArrow "a" (TyArrow "b" "c"))
  subAssert $ assertParseTy "a -> (b -> c)" (TyArrow "a" (TyArrow "b" "c"))
  subAssert $ assertParseTy "(a -> b) -> c" (TyArrow (TyArrow "a" "b") "c")
  subAssert $ assertParseTy "P Int a -> Int"
    (TyArrow (TyConstr "P" [TyPrim PrimInt, "a"]) (TyPrim PrimInt))
  where
    assertParseTy src expected = do
      ty <- parseOrDie parseTy src
      assertEqual expected ty

test_parseExp :: IO ()
test_parseExp = do
  subAssert $ assertParsePatClause
    "Pair @Int a { x: Int, _: a } -> x" $
    PatClause
      (PatConstr (ConstrName "Pair") [TyPrim PrimInt, TyVar "a"]
         [PatVar "x" (TyPrim PrimInt),
          PatWild (TyVar "a")])
      (ExpVar "x")
  subAssert $ assertParseExp
    "case p of Pair @Int a { x: Int, _: a } -> x" $
    ExpCase (ExpVar "p")
      [PatClause
         (PatConstr (ConstrName "Pair") [TyPrim PrimInt, TyVar "a"]
            [PatVar "x" (TyPrim PrimInt),
             PatWild (TyVar "a")])
         (ExpVar "x")]
  where
    assertParseExp src expected = do
      ty <- parseOrDie parseExp src
      assertEqual expected ty
    assertParsePatClause src expected = do
      ty <- parseOrDie parsePatClause src
      assertEqual expected ty
