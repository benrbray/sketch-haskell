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

data Name
  = Name Text
  | Fresh Int
  deriving stock (Show, Eq, Ord)

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
  = TV Name
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

ops :: Map.Map Binop Type
ops = Map.fromList [
      (Add, (typeInt `TypeArr` (typeInt `TypeArr` typeInt)))
    , (Mul, (typeInt `TypeArr` (typeInt `TypeArr` typeInt)))
    , (Sub, (typeInt `TypeArr` (typeInt `TypeArr` typeInt)))
    , (Eql, (typeInt `TypeArr` (typeInt `TypeArr` typeBool)))
  ]

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
  | UnboundVariable Name
  deriving stock (Show)

type Infer a = ExceptT TypeError (Gen Int) a

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

--------------------------------------------------------------------------------

freshTV :: Infer TV
freshTV = TV . Fresh <$> gen

instantiate :: Scheme -> Infer Type
instantiate (Forall as0 t) = do
  -- bind fresh typevars for each typevar mentioned in the forall
  as1 <- traverse (const $ TypeVar <$> freshTV) as0
  let s = Map.fromList $ zip as0 as1
  return $ applySubst s t

-- universally quantify over any free variables in a given type
-- (which are not also mentioned in the typing environment)
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ freeTypeVars t `Set.difference` freeTypeVars env

--------------------------------------------------------------------------------

runInfer :: Infer (Subst, Type) -> Either TypeError (Subst, Type)
runInfer m = case runGen (runExceptT m) of
  Left err -> Left err
  Right y -> Right y

-- looks up a local variable in the typing env,
-- and if found instantiates a fresh copy
-- QUESTION: why return Subst here if it's always empty?
-- QUESTION: what's the meaning of the (Subst, Type) tuple?
lookupEnv :: TypeEnv -> Name -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing  -> throwError $ UnboundVariable x
    Just sch -> do
      t <- instantiate sch
      return (emptySubst, t)

-- maps the local typing env and the active expression to a tuple containing
--     a partial solution to unification
--     the intermediate type
-- the AST is traversed bottom-up and constraints are solved at each level of
-- recursion by applying partial substitutions from unification across each
-- partially inferred subexpression and the local environment
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env0 ex = case ex of
  
  Var x -> lookupEnv env0 x

  Lam x e -> do
    tv <- TypeVar <$> freshTV
    let env1 = extend env0 (x, Forall [] tv)
    (s1, t1) <- infer env1 e
    return (s1, applySubst s1 tv `TypeArr` t1)
  
  App e1 e2 -> do
    tv <- TypeVar <$> freshTV
    (s1, t1) <- infer env0 e1
    (s2, t2) <- infer (applySubst s1 env0) e2
    s3       <- unify (applySubst s2 t1) (TypeArr t2 tv)
    return (s3 `merge` s2 `merge` s1, applySubst s3 tv)

  Let x e1 e2 -> do
    (s1, t1a) <- infer env0 e1
    let env1  = (applySubst s1 env0)
        t1b   = generalize env1 t1a
        env2  = env1 `extend` (x, t1b)
    (s2, t2)  <- infer env2 e2
    return (s2 `merge` s1, t2)

  If cond thn els -> do
    (s1, t1) <- infer env0 cond
    (s2, t2) <- infer env0 thn
    (s3, t3) <- infer env0 els
    s4       <- unify t1 typeBool
    s5       <- unify t2 t3
    return (s5 `merge` s4 `merge` s3 `merge` s2 `merge` s1, applySubst s5 t2)

  Fix e1 -> do
    (s1, t) <- infer env0 e1
    tv <- TypeVar <$> freshTV
    s2 <- unify (TypeArr tv tv) t
    return (s2, applySubst s1 tv)
  
  Op op e1 e2 -> do
    (s1, t1) <- infer env0 e1
    (s2, t2) <- infer env0 e2
    tv <- TypeVar <$> freshTV
    s3 <- unify (TypeArr t1 (TypeArr t2 tv)) (ops Map.! op)
    return (s1 `merge` s2 `merge` s3, applySubst s3 tv)

  Lit (LInt _)  -> return (emptySubst, typeInt)
  Lit (LBool _) -> return (emptySubst, typeBool)