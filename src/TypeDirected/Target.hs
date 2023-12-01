module TypeDirected.Target where

import qualified Common.FGAST as A
import qualified Data.Text as T

-- Target language supporting existentials and iso-recursive types.

type TyName = T.Text
type VarName = T.Text

data Type
  = Var TyName
  | Tuple [Type]
  | Func Type Type
  | Exists TyName Type
  | Mu Int [TyName] [Type]
  deriving (Show, Eq) -- starts with 1

data Exp
  = VarExp VarName
  | -- Haskell backend:
    -- Need to distinguish between tuples resulting from strucutres and
    -- tuples we use internally, like the tuple to describe the components of an interfaces.
    -- If Nothing then internal tuple, otherwise the type name refers to the structure name.
    TupleExp
      { tupleName :: Maybe A.TyName
      , tupleExps :: [Exp]
      }
  | -- Haskell backend:
    -- See TupleExp above.
    -- projIdex of form (index, tuple length) necessary for Haskell backend as well.
    Proj
      { projName :: Maybe A.TyName
      , projIdxs :: (Int, Int)
      , projExps :: Exp
      }
  | App Exp Exp
  | Lambda VarName Type Exp
  | Letrec [(VarName, Type, Exp)] Exp
  | -- Haskell backend:
    -- packD names of interfaces to support translation of target to Haskell
    -- packT is the type and packD is the (interface) name of the type
    -- From packT alone we wouldn't be able to figure out the name.
    Pack {pack :: (Type, Exp), packT :: Type, packD :: TyName}
  | Unpack {unpack :: (TyName, VarName), unpackE1 :: Exp, unpackE2 :: Exp, unpackD :: TyName}
  | Fold Type Exp
  | Unfold Type Exp
  deriving (Show)

-- | Plain projection
mkProj :: (Int, Int) -> Exp -> Exp
mkProj (i, n) e =
  Proj
    { projName = Nothing
    , projIdxs = (i, n)
    , projExps = e
    }

-- | Plain tuple
mkTuple :: [Exp] -> Exp
mkTuple es =
  TupleExp
    { tupleName = Nothing
    , tupleExps = es
    }

subst :: [(TyName, Type)] -> Type -> Type
subst phi t = subst2 [] phi t

-- | xs list of bound variables that shall not be substituted
subst2 :: [TyName] -> [(TyName, Type)] -> Type -> Type
subst2 xs phi (Var x)
  | x `elem` xs = Var x
  | otherwise = case lookup x phi of
      Just t -> t
      Nothing -> Var x
subst2 xs phi (Tuple ts) = Tuple $ map (subst2 xs phi) ts
subst2 xs phi (Func s t) = Func (subst2 xs phi s) (subst2 xs phi t)
subst2 xs phi (Exists x t) = Exists x $ subst2 (x : xs) phi t
subst2 xs phi (Mu i ys ts) = Mu i ys $ map (subst2 (xs ++ ys) phi) ts

expand :: Type -> Type
expand (Mu i xs ts)
  | length xs == length ts
      && i >= 0
      && i <= length xs =
      let phi = zip xs ts
       in subst phi (ts !! (i - 1))
  | otherwise = error "expand: Mu impossible"
expand _ = error "expand: impossible"
