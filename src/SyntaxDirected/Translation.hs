{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module SyntaxDirected.Translation where

import qualified Common.FGAST as G
import Common.HaskellAST
import Common.HaskellPretty ()
import Common.Types
import Common.Utils

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Data.Char (isLower)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

langExtensions :: T.Text
langExtensions =
  T.unlines
    [ "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE UndecidableInstances #-}"
    , "{-# LANGUAGE FunctionalDependencies #-}"
    , "{-# LANGUAGE ExistentialQuantification #-}"
    , "{-# LANGUAGE GADTs #-}"
    , "{-# LANGUAGE QuantifiedConstraints #-}"
    , "{-# LANGUAGE BangPatterns #-}"
    , "{-# LANGUAGE NoMonomorphismRestriction #-}"
    , "{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}"
    ]

prelude :: T.Text
prelude =
  T.unlines
    [ "type Type_bool = Bool"
    , "v_true = True"
    , "v_false = False"
    , "type Type_int = Int"
    , "type Type_rune = Char"
    , "type Type_string = String"
    , "force :: forall a . Show a => a -> a"
    , "force x = length (show x) `seq` x"
    ]

data TypeKind = KindStruct | KindIface | KindBuiltin

data TransEnv = TransEnv
  { te_casts :: WithCasts
  , te_structs :: Set G.TyName
  }

data TransState = TransState
  { ts_nextFreshVar :: Int
  , ts_nextFreshTyVar :: Int
  }

newtype T a = T {unT :: RWS TransEnv () TransState a}
  deriving (Functor, Applicative, Monad, MonadReader TransEnv, MonadState TransState)

ifCast :: a -> T a -> T a
ifCast def action = do
  env <- ask
  case te_casts env of
    WithCasts -> action
    WithoutCasts -> pure def

freshVar :: T VarName
freshVar = do
  s <- get
  let i = ts_nextFreshVar s
  put (s{ts_nextFreshVar = i + 1})
  return $ VarName (T.pack ("x" ++ show i))

freshTyVar :: T TyVarName
freshTyVar = do
  s <- get
  let i = ts_nextFreshTyVar s
  put (s{ts_nextFreshTyVar = i + 1})
  return $ TyVarName (T.pack ("a" ++ show i))

builtinTypes :: Set.Set G.TyName
builtinTypes = Set.fromList ["int", "bool", "rune", "string"]

classifyType :: G.TyName -> T TypeKind
classifyType t = do
  env <- ask
  pure $
    if
        | t `Set.member` te_structs env -> KindStruct
        | t `Set.member` builtinTypes -> KindBuiltin
        | otherwise -> KindIface

runTrans :: WithCasts -> G.Program -> Prog
runTrans casts prog =
  let structs = allStructs (G.p_decls prog)
      env = TransEnv casts structs
      state = TransState 0 0
   in fst $ runIdentity $ evalRWST (unT $ transProg prog) env state

translate :: WithCasts -> T.Text -> G.Program -> T.Text
translate casts header prog =
  let hsProg = runTrans casts prog
   in prettyToText (langExtensions <> "\n\n" <> header <> "\n\n" <> prelude <> "\n\n") hsProg

--

-- |- P ~> prog

-- SD-PROG
transProg :: G.Program -> T Prog
transProg prog = do
  let fields = allFields (G.p_decls prog)
      methodNames = allMeNames (G.p_decls prog)
  decls <- concatMapM transDecl (G.p_decls prog)
  decls' <- ifCast [] $ transCastDecls (G.p_decls prog)
  main <- transMain (G.p_main prog)
  return $
    Prog
      { p_decls =
          [ DeclClass $
              ClassDecl
                ["r", "f", "t"]
                []
                "HasField"
                []
                [MeDecl "field" (simpleSigma $ tv "r" `to` tv "f" `to` tv "t")]
          , DeclClass $
              ClassDecl
                ["recv", "name", "args", "res"]
                []
                "Method"
                []
                [ MeDecl
                    "apply"
                    ( simpleSigma $
                        tv "recv" `to` tv "name" `to` tv "args" `to` tv "res"
                    )
                ]
          , DeclClass $
              ClassDecl
                ["recv", "name", "res"]
                []
                "InferRes"
                [FunDep ["recv", "name"] "res"]
                [ MeDecl
                    "inferRes"
                    ( simpleSigma $
                        tv "recv" `to` tv "name" `to` tv "res" `to` tv "res"
                    )
                ]
          , DeclClass $
              ClassDecl
                ["a", "b"]
                []
                "Convert"
                [FunDep ["a"] "b"]
                [MeDecl "convert" (simpleSigma $ tv "a" `to` tv "b")]
          ]
            ++ map
              (\f -> DeclData $ DataDecl (nameFty f) [] [simpleCtor (nameFda f)] [])
              (Set.toList fields)
            ++ map
              (\m -> DeclData $ DataDecl (nameMty m) [] [simpleCtor (nameMda m)] [])
              (Set.toList methodNames)
            ++ decls
            ++ decls'
      , p_main = main
      }

transMain :: G.Main -> T Main
transMain main = do
  bindings <-
    Control.Monad.forM (G.m_bindings main) $ \(x, e) -> do
      e' <- transExp e
      return (nameV x, e')
  res <- transExp (G.m_result main)
  return $ Main bindings res

collectAll :: Ord a => (G.Decl -> [a]) -> [G.Decl] -> Set a
collectAll f decls = foldr add Set.empty decls
 where
  add decl acc =
    acc `Set.union` Set.fromList (f decl)

allFields :: [G.Decl] -> Set G.FieldName
allFields = collectAll get
 where
  get (G.Type _ (G.Struct fields)) = map fst fields
  get _ = []

allMeNames :: [G.Decl] -> Set G.MeName
allMeNames = collectAll get
 where
  get (G.Type _ (G.Iface methods)) = map G.ms_name methods
  get (G.Method _ (G.MeSpec m _) _) = [m]
  get _ = []

allStructs :: [G.Decl] -> Set G.TyName
allStructs = collectAll get
 where
  get (G.Type t (G.Struct _)) = [t]
  get _ = []

allTypeDecls :: [G.Decl] -> Set G.TyName
allTypeDecls = collectAll get
 where
  get (G.Type t (G.TypeDecl _)) = [t]
  get _ = []

allMethodSpecs :: [G.Decl] -> Set G.MeSpec
allMethodSpecs = collectAll get
 where
  get (G.Type _ (G.Iface specs)) = specs
  get (G.Method _ spec _) = [spec]
  get _ = []

--

-- |- D ~> \overline{decl}
transDecl :: G.Decl -> T [Decl]
transDecl decl =
  case decl of
    G.Type t (G.Struct fields) -> transStructDecl t fields
    G.Type t (G.Iface methods) -> transIfaceDecl t methods
    G.Type t (G.TypeDecl tyName) -> transTypeDecl t tyName
    G.Method recv spec exp -> transMeDecl recv spec exp

-- SD-STRUCT-DEC
transStructDecl :: G.TyName -> [(G.FieldName, G.TyName)] -> T [Decl]
transStructDecl t fields = do
  let tyCon = nameTy t
      dataCon = nameDat t
      ys = take (length fields) (map (\i -> VarName ("Y" <> showText i)) [1 ..])
  es <-
    Control.Monad.forM (zip fields ys) $ \((_, ti), yi) -> do
      kind <- classifyType ti
      case kind of
        KindStruct -> return (ExpVar yi)
        KindBuiltin -> return (ExpVar yi)
        KindIface -> return $ ExpApp (ExpDataCon (nameDat ti)) (ExpVar yi)
  toStruct <-
    ifCast [] $
      pure
        [ DeclInst $
            InstDecl
              noInstFlags
              Set.empty
              []
              "ToStruct"
              [TypeTyCon tyCon []]
              [MethodDef "toStruct" (ExpDataCon (nameUniv t))]
        ]
  inferInsts <-
    Control.Monad.forM fields $ \(fi, ti) ->
      transInferRes (simpleTyCon tyCon) (simpleTyCon (nameFty fi)) (simpleTyCon (nameTy ti))
  return $
    [ DeclData $
        DataDecl
          tyCon
          []
          [Constructor dataCon [] [] (map (\(_, ti) -> simpleTyCon (nameTy ti)) fields) Nothing]
          ["Show"]
    ]
      ++ map DeclInst inferInsts
      ++ ( flip map (zip [0 ..] fields) $ \(i, (fi, ti)) ->
            DeclInst $
              InstDecl
                noInstFlags
                Set.empty
                []
                "HasField"
                [simpleTyCon tyCon, simpleTyCon (nameFty fi), simpleTyCon (nameTy ti)]
                [ MethodDef "field" $
                    ExpLambda "X" $
                      ExpLambda "X'" $
                        ExpCase (ExpVar "X") [(Pat dataCon ys, ExpVar (ys !! i))]
                ]
         )
      ++ toStruct
      ++ [DeclVal $ ValDecl Nothing (nameMake t) ys $ foldl ExpApp (ExpDataCon dataCon) es]

-- SD-IFACE-DEC
transIfaceDecl :: G.TyName -> [G.MeSpec] -> T [Decl]
transIfaceDecl t methods = do
  let className = nameCls t
      tyCon = nameTy t
      k = nameDat t
      alpha = TyVarName "a"
  pis <- mapM (\spec -> transMethodConstraint spec (TypeTyVar alpha)) methods
  insts <- concatMapM (\spec -> transMethodInstance spec tyCon k) methods
  (toStructC, toStructI) <-
    ifCast ([], []) $
      pure
        ( [simpleConstr (ClassConstr "ToStruct" [TypeTyVar "a"])]
        ,
          [ DeclInst $
              InstDecl
                noInstFlags
                Set.empty
                []
                "ToStruct"
                [TypeTyCon tyCon []]
                [ MethodDef "toStruct" $
                    ExpLambda
                      "X"
                      ( ExpCase
                          (ExpVar "X")
                          [(Pat k ["Y"], ExpApp (ExpVar "toStruct") (ExpVar "Y"))]
                      )
                ]
          ]
        )
  let showC = simpleConstr (ClassConstr "Show" [TypeTyVar "a"])
  return $
    [ DeclClass $ ClassDecl ["a"] (showC : toStructC ++ pis) className [] []
    , DeclInst $
        InstDecl
          noInstFlags
          (Set.singleton "a")
          (showC : toStructC ++ pis)
          className
          [TypeTyVar "a"]
          []
    , DeclData $
        DataDecl
          tyCon
          []
          [ Constructor
              k
              ["b"]
              [simpleConstr (ClassConstr className [TypeTyVar "b"])]
              [TypeTyVar "b"]
              Nothing
          ]
          []
    , DeclInst $
        InstDecl
          noInstFlags
          Set.empty
          []
          "Show"
          [simpleTyCon tyCon]
          [ MethodDef "show" $
              ExpLambda "X" $
                ExpCase
                  (var "X")
                  [(Pat k ["Y"], var "show" `app` var "Y")]
          ]
    ]
      ++ map DeclInst insts
      ++ toStructI

transTypeDecl :: G.TyName -> G.TyName -> T [Decl]
transTypeDecl newTy existingTy = do
  let tyCon = nameTy newTy
      tyConTxt = unTyConName tyCon
      dat = nameDat newTy
      unDat = nameUn newTy
  return
    [ DeclNewtype $ NewtypeDecl tyCon dat unDat (nameTy existingTy)
    , DeclInst $
        InstDecl
          noInstFlags
          Set.empty
          []
          "Convert"
          [simpleTyCon tyCon, simpleTyCon (nameTy existingTy)]
          [MethodDef "convert" (var unDat)]
    , DeclInst $
        InstDecl
          noInstFlags
          Set.empty
          []
          "Convert"
          [simpleTyCon (nameTy existingTy), simpleTyCon tyCon]
          [MethodDef "convert" (ExpDataCon dat)]
    , DeclVal $
        ValDecl
          (Just ("Convert a " <> tyConTxt <> " => a -> " <> tyConTxt))
          (nameConvert newTy)
          []
          (var "convert")
    ]

-- SD-METHOD-DEC
transMeDecl
  :: (G.VarName, G.TyName)
  -> G.MeSpec
  -> G.Exp
  -> T [Decl]
transMeDecl
  (xg, tg)
  spec@(G.MeSpec m (G.MeSig args res))
  expG = do
    let xis = map (\(x, _) -> nameV x) args
        x = nameV xg
    x' <- freshVar
    y <- freshVar
    yis <- mapM (\_ -> freshVar) args
    bindings <-
      Control.Monad.forM (zip3 xis yis args) $ \(xi, yi, (_, ti)) -> do
        ki <- classifyType ti
        let ei =
              case ki of
                KindIface -> ExpApp (ExpDataCon (nameDat ti)) (ExpVar yi)
                KindStruct -> ExpVar yi
                KindBuiltin -> ExpVar yi
        pure (xi, ei)
    Constr tyvars context (ClassConstr className argTys) <-
      transMethodConstraint spec (TypeTyCon (nameTy tg) [])
    inferInst <-
      transInferRes (simpleTyCon $ nameTy tg) (simpleTyCon $ nameMty m) (simpleTyCon $ nameTy res)
    exp' <- transExp expG
    kind <- classifyType res
    let exp =
          case kind of
            KindIface -> ExpApp (ExpDataCon (nameDat res)) exp'
            KindStruct -> exp'
            KindBuiltin -> exp'
    return
      [ DeclInst $
          InstDecl
            noInstFlags
            tyvars
            (map simpleConstr context)
            className
            argTys
            [ MethodDef "apply" $
                ExpLambda x $
                  ExpLambda x' $
                    ExpLambda y $
                      ExpCase (ExpVar y) [(Pat (tupleDataCon (length args)) yis, ExpLet bindings exp)]
            ]
      , DeclInst inferInst
      ]

--

{- |- \overline{D} ~> \overline{decl}

 (CAST ONLY)
 SD-CAST
-}
transCastDecls :: [G.Decl] -> T [Decl]
transCastDecls goDecls = do
  let structs = Set.toList $ allStructs goDecls
      tys = Set.toList $ allTypeDecls goDecls
      specs = Set.toList $ allMethodSpecs goDecls
  decls <- mapM (transCastDecl goDecls) goDecls
  decls' <- mapM transSpecWitness specs
  return $
    [ DeclClass $
        ClassDecl
          ["a"]
          []
          "ToStruct"
          []
          [MeDecl "toStruct" (simpleSigma $ tv "a" `to` TypeTyCon "AnyStruct" [])]
    , DeclData $
        DataDecl
          "AnyStruct"
          []
          ( flip map (structs ++ tys) $ \t ->
              Constructor (nameUniv t) [] [] [TypeTyCon (nameTy t) []] Nothing
          )
          []
    ]
      ++ concat decls
      ++ concat decls'

-- |- \overline{D}; D ~> \overline{decl}
transCastDecl :: [G.Decl] -> G.Decl -> T [Decl]
transCastDecl decls decl =
  case decl of
    G.Type t (G.Struct _) -> transStructCastDecl decls t
    G.Type t (G.Iface specs) -> transIfaceCastDecl decls t specs
    G.Type t (G.TypeDecl _) -> transStructCastDecl decls t
    G.Method (_, t) spec _ -> transMethodCastDecl t spec

-- SD-STRUCT-CAST
transStructCastDecl :: [G.Decl] -> G.TyName -> T [Decl]
transStructCastDecl _goDecls t =
  return
    [ DeclVal $
        ValDecl Nothing (nameCast t) [] $
          ExpLambda "X" $
            ExpCase
              (ExpApp (ExpVar "toStruct") (ExpVar "X"))
              [(Pat (nameUniv t) ["Y"], ExpVar "Y")]
    ]

-- SD-IFACE-CAST
transIfaceCastDecl :: [G.Decl] -> G.TyName -> [G.MeSpec] -> T [Decl]
transIfaceCastDecl goDecls t specs = do
  let structs = Set.toList $ allStructs goDecls
      castAux =
        ExpLambda "X" $
          foldr makeWitnessCase (ExpApp (ExpDataCon (nameDat t)) (ExpVar "X")) specs
  return
    [ DeclVal $
        ValDecl Nothing (nameCast t) [] $
          ExpLambda "X" $
            ExpLet [("castAux", castAux)] $
              ExpCase
                (ExpApp (ExpVar "toStruct") (ExpVar "X"))
                ( flip map structs $ \ti ->
                    (Pat (nameUniv ti) ["X"], ExpApp (ExpVar "castAux") (ExpVar "X"))
                )
    ]
 where
  makeWitnessCase si exp =
    ExpCase
      (ExpApp (ExpVar (nameMsv si)) (ExpVar "X"))
      [(Pat (nameMsda si) ["X"], exp)]

-- SD-METHOD-CAST
transMethodCastDecl :: G.TyName -> G.MeSpec -> T [Decl]
transMethodCastDecl t spec =
  let decls =
        [ DeclInst $
            InstDecl
              noInstFlags
              Set.empty
              []
              (nameMscls spec)
              [simpleTyCon (nameTy t)]
              [MethodDef (nameMsv spec) $ ExpDataCon (nameMsda spec)]
        ]
   in ifCast [] (pure decls)

expCastError :: T.Text -> Exp
expCastError msg = ExpVar "error" `app` ExpStrLit ("cast error: " <> msg)

{- |- S ~>_wit \overline{decl}
 SD-METHOD-WITNESS
-}
transSpecWitness :: G.MeSpec -> T [Decl]
transSpecWitness spec = do
  let a = TyVarName "a"
      tyCon = nameMsty spec
      className = nameMscls spec
      methodName = nameMsv spec
  pi <- transMethodConstraint spec (tv a)
  return
    [ DeclData $
        DataDecl
          tyCon
          [a]
          [Constructor (nameMsda spec) [] [pi] [tv a] Nothing]
          []
    , DeclClass $
        ClassDecl
          ["a"]
          []
          className
          []
          [MeDecl methodName (simpleSigma $ tv a `to` TypeTyCon tyCon [tv a])]
    , DeclInst $
        InstDecl
          overlappableFlags
          (Set.singleton a)
          []
          className
          [tv a]
          [ MethodDef methodName $
              ExpLambda "Y" $
                expCastError ("method not implemented " <> showText spec)
          ]
    ]

{- |- S; T; K ~>_mi \overline{inst}
 SD-IFACE-METHOD
-}
transMethodInstance
  :: G.MeSpec
  -> TyConName
  -> DataConName
  -> T [InstDecl]
transMethodInstance spec tyCon datCon = do
  Constr tyVars ctx (ClassConstr className taus) <-
    transMethodConstraint spec (simpleTyCon tyCon)
  let decl =
        InstDecl
          noInstFlags
          tyVars
          (map simpleConstr ctx)
          className
          taus
          [ MethodDef "apply" $
              ExpLambda "X1" $
                ExpLambda "X2" $
                  ExpLambda "X3" $
                    ExpCase
                      (ExpVar "X1")
                      [
                        ( Pat datCon ["X1'"]
                        , var "apply" `app` var "X1'" `app` var "X2" `app` var "X3"
                        )
                      ]
          ]
  inferInst <-
    transInferRes
      (simpleTyCon tyCon)
      (simpleTyCon $ nameMty (G.ms_name spec))
      (simpleTyCon $ nameTy $ G.ms_result spec)
  pure [decl, inferInst]

{- |- S; \tau ~>_mc \pi
 SD-METHOD-SPEC
-}
transMethodConstraint :: G.MeSpec -> Tau -> T Constr
transMethodConstraint (G.MeSpec m (G.MeSig args u)) tau = do
  csAndTs <- mapM (\(_, tj) -> transParamType tj) args
  let cs = mapMaybe fst csAndTs
      ts = map snd csAndTs
      alphas = freeTyVars ts
  return $
    Constr alphas cs $
      ClassConstr
        "Method"
        [tau, simpleTyCon (nameMty m), TypeTyCon (tupleTyCon (length ts)) ts, simpleTyCon (nameTy u)]

{- |- \tau_1; \tau_2; \tau_3 ~>_ir inst
 SD-INFER-RES
-}
transInferRes :: Tau -> Tau -> Tau -> T InstDecl
transInferRes recv name res =
  pure $
    InstDecl
      noInstFlags
      Set.empty
      []
      "InferRes"
      [recv, name, res]
      [ MethodDef "inferRes" $
          ExpLambda "X1" $
            ExpLambda "X2" $
              ExpLambda
                "X3"
                (ExpVar "X3")
      ]

{- |- t ~>_p \overline{\chi}; \tau
 SD-PARAM-IFACE / SD-PARAM-STRUCT
-}
transParamType :: G.TyName -> T (Maybe ClassConstr, Tau)
transParamType t = do
  kind <- classifyType t
  case kind of
    KindStruct -> return (Nothing, simpleTyCon (nameTy t))
    KindBuiltin -> return (Nothing, simpleTyCon (nameTy t))
    KindIface -> do
      a <- freshTyVar
      return (Just (ClassConstr (nameCls t) [tv a]), tv a)

-- |- e ~> E
transExp :: G.Exp -> T Exp
transExp exp =
  case exp of
    G.Var x -> return $ ExpVar (nameV x)
    G.MeCall e m es -> do
      es' <- mapM transExp es
      e' <- transExp e
      let k = ExpDataCon (nameMda m)
          apply = var "apply" `app` e' `app` k `app` tupleExp es'
      return $ var "inferRes" `app` e' `app` k `app` apply
    G.StructLit t es -> do
      es' <- mapM transExp es
      return $ foldl ExpApp (ExpVar (nameMake t)) es'
    G.Select e f -> do
      e' <- transExp e
      let k = ExpDataCon (nameFda f)
          field = var "field" `app` e' `app` k
      return $ var "inferRes" `app` e' `app` k `app` field
    G.TyAssert e t -> do
      e' <- transExp e
      return $ var (nameCast t) `app` e'
    G.IntLit i -> return $ ExpIntLit i
    G.CharLit i -> return $ ExpCharLit i
    G.StrLit s -> return $ ExpStrLit s
    G.BinOp o l r -> do
      l' <- transExp l
      r' <- transExp r
      return (ExpBinOp o l' r')
    G.UnOp o e -> do
      e' <- transExp e
      return (ExpUnOp o e')
    G.Cond c t f -> do
      c' <- transExp c
      t' <- transExp t
      f' <- transExp f
      return (ExpIte c' t' f')

nameFty :: G.FieldName -> TyConName
nameFty (G.FieldName s) = TyConName ("FieldType_" <> s)

nameFda :: G.FieldName -> DataConName
nameFda (G.FieldName s) = DataConName ("Field_" <> s)

nameMty :: G.MeName -> TyConName
nameMty (G.MeName m) = TyConName ("MethodType_" <> m)

nameMda :: G.MeName -> DataConName
nameMda (G.MeName m) = DataConName ("Method_" <> m)

nameTy :: G.TyName -> TyConName
nameTy (G.TyName t) = TyConName ("Type_" <> t)

nameDat :: G.TyName -> DataConName
nameDat (G.TyName t) = DataConName ("Mk_" <> t)

nameUniv :: G.TyName -> DataConName
nameUniv (G.TyName t) = DataConName ("Is_" <> t)

nameCls :: G.TyName -> ClassName
nameCls (G.TyName t) = ClassName ("C_" <> t)

nameV :: G.VarName -> VarName
nameV (G.VarName x) = if x == "_" then VarName "_" else VarName ("v_" <> x)

nameMake :: G.TyName -> VarName
nameMake (G.TyName t) = VarName ("make_" <> t)

nameCast :: G.TyName -> VarName
nameCast (G.TyName t) = VarName ("cast_" <> t)

-- We assume that "__" does not occur in names of methods or types in Go.
stringForMethodSpec :: G.MeSpec -> T.Text
stringForMethodSpec (G.MeSpec (G.MeName m) (G.MeSig as (G.TyName u))) =
  T.intercalate "__" (m : map argString as ++ [u])
 where
  argString (_, G.TyName t) = t

nameMsv :: G.MeSpec -> VarName
nameMsv spec = VarName ("getMethod_" <> stringForMethodSpec spec)

nameMscls :: G.MeSpec -> ClassName
nameMscls spec = ClassName ("GetMethod_" <> stringForMethodSpec spec)

nameMsda :: G.MeSpec -> DataConName
nameMsda spec = DataConName ("MkWitnessMethod_" <> stringForMethodSpec spec)

nameMsty :: G.MeSpec -> TyConName
nameMsty spec = TyConName ("WitnessMethod_" <> stringForMethodSpec spec)

nameUn :: G.TyName -> VarName
nameUn t = VarName ("un" <> unTyConName (nameTy t))

nameConvert :: G.TyName -> VarName
nameConvert (G.unTyName -> t) =
  VarName $
    case T.uncons t of
      Just (c, _) | isLower c -> t
      _ -> "c_" <> t
