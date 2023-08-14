-- Stephen Diehl
-- https://web.archive.org/web/20181017074008/http://dev.stephendiehl.com/fun/006_hindley_milner.html

{-# LANGUAGE OverloadedStrings #-}

module HindleyMilner where

import Protolude hiding (Type, TypeError(TypeError))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Control.Monad.Gen
import Control.Monad.Trans.Except
import GHC.Base (error)

---- syntax --------------------------------------------------------------------

type Name = Int

data Expr
  = Var Name            -- variable
  | App Expr Expr       -- application
  | Lam Name Expr       -- lambda
  | Let Name Expr Expr  -- let x = e1 in e2
  | Lit Lit             -- literal
  | If Expr Expr Expr   -- if then else
  | Fix Expr            -- fixpoint
  | Op Binop Expr Expr  -- binary operator
  deriving stock (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving stock (Show, Eq, Ord)

data Binop
  = Add | Sub | Mul | Eql
  deriving stock (Show, Eq, Ord)

type Decl
  = (Text, Expr)

data Program
  = Program [Decl] Expr
  deriving stock (Show, Eq, Ord)

---- types ---------------------------------------------------------------------

-- type variable
newtype TV
  = TV Text
  deriving stock (Show, Eq, Ord)

data Type
  = TypeVar TV        -- type variable
  | TypeCon Text    -- type constructor
  | TypeArr Type Type -- arrow type
  deriving stock (Show, Eq, Ord)

typeInt :: Type
typeInt = TypeCon "Int"

typeBool :: Type
typeBool = TypeCon "Bool"

-- type scheme models polymorphic types
data Scheme = Forall [TV] Type

---- context -------------------------------------------------------------------

newtype TypeEnv
  = TypeEnv (Map Name Scheme)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) =
  if Map.member x env
    then error $ "duplicate element in type env: " ++ (show x)
    else TypeEnv $ Map.insert x s env

---- substitution --------------------------------------------------------------

-- substitution is only performed on type variables
type Subst = Map TV Type

class Substitutable a where
  applySubst :: Subst -> a -> a
  freeTypeVars :: a -> Set TV

instance Substitutable Type where
  applySubst :: Subst -> Type -> Type
  applySubst _ t@(TypeCon _)   = t
  applySubst s t@(TypeVar x)   = Map.findWithDefault t x s
  applySubst s (TypeArr t1 t2) = TypeArr (applySubst s t1) (applySubst s t2)

  freeTypeVars :: Type -> Set TV
  freeTypeVars (TypeCon _) = Set.empty
  freeTypeVars (TypeVar x) = Set.singleton x
  freeTypeVars (TypeArr t1 t2) = freeTypeVars t1 <> freeTypeVars t2

instance Substitutable Scheme where
  applySubst s0 (Forall tvs t)  = Forall tvs $ applySubst s1 t
    where
      -- vars bound by forall shadow vars in the context
      s1 = foldr Map.delete s0 tvs
  
  freeTypeVars :: Scheme -> Set TV
  freeTypeVars (Forall tvs t)
    = freeTypeVars t `Set.difference` Set.fromList tvs

instance Substitutable a => Substitutable [a] where
  applySubst = fmap . applySubst
  freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty

instance Substitutable TypeEnv where
  applySubst s (TypeEnv env) = TypeEnv $ Map.map (applySubst s) env
  freeTypeVars (TypeEnv env) = freeTypeVars $ Map.elems env

emptySubst :: Subst
emptySubst = Map.empty

-- before merging, first apply @s1@ to all types mentioned in @s2@
-- QUESTION: in practice, is it important to allow overlap?  can we do without?
merge :: Subst -> Subst -> Subst
s1 `merge` s2 = (Map.map (applySubst s1) s2) `Map.union` s1 

---- inference monad -----------------------------------------------------------

data TypeError
  = InfiniteType TV Type
  | CannotUnify Type Type
  | TypeErrorOther Text
  deriving stock (Show)

type Infer a = ExceptT TypeError (Gen Name) a

-- runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer :: Infer (Subst, Type) -> Either TypeError (Subst, Type)
runInfer m = case runGen (runExceptT m) of
  Left err -> Left err
  Right y -> Right y

---- first order unification ---------------------------------------------------

-- the result of unification is either
--     a unifying substitution,
--     or a type error indicating failure
unify :: Type -> Type -> Infer Subst
unify (TypeVar x) t = unifyBind x t
unify t (TypeVar x) = unifyBind x t
unify (TypeCon c1) (TypeCon c2)
  | c1 == c2 = pure emptySubst
unify (TypeArr l1 r1) (TypeArr l2 r2) = do
  sL <- unify l1 l2
  sR <- unify (applySubst sL r1) (applySubst sL r2)

  -- QUESTION: why not @sL `merge` sR@?
  pure (sR `merge` sL)
unify t1 t2 = throwError $ CannotUnify t1 t2

-- returns @True@ whenever the type variable @x@ appears free in @t@
-- used to rule out infinite types, e.g. when checking @λx. x x@
occursCheck :: Substitutable a => TV -> a -> Bool
occursCheck x t = x `Set.member` freeTypeVars t

unifyBind :: TV -> Type -> Infer Subst
unifyBind x t
  | t == (TypeVar x) = pure emptySubst
  | occursCheck x t  = throwError $ InfiniteType x t
  | otherwise        = return $ Map.singleton x t

