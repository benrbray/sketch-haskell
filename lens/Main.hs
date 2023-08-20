{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Text (Text)
import Prelude
import Control.Lens (Traversal', over, transformOf, Traversal)
import Control.Monad.Writer
import Data.Functor.Identity (Identity(..))

--------------------------------------------------------------------------------

main :: IO ()
main = do
  print example1
  print $ globalSqrt example1

  print example2
  print $ globalSqrt example2
  pure ()

--------------------------------------------------------------------------------

-- https://www.michaelpj.com/blog/2020/08/02/lenses-for-tree-traversals.html

type Name = Text

data Type
  = IntegerType
  | FunType Type Type
  deriving (Eq, Show)

data Term
  = Var Name
  | Lam Name Type Term
  | App Term Term
  | Plus Term Term
  | Constant Integer
  | Sqrt Term
  deriving (Eq, Show)

varX = Var "X"
varY = Var "Y"
varZ = Var "Z"

const0 = Constant 0

example1 :: Term
example1 = Sqrt varX

example2 :: Term
example2 = Plus varX $ Sqrt (App varZ const0)

--------------------------------------------------------------------------------

data SqrtVar
  = SqrtVar Name Term
  deriving (Eq, Show)

locallyReplaceSqrt :: Term -> (Term, [SqrtVar])
locallyReplaceSqrt (Sqrt t) = (Sqrt (Var "foo"), [SqrtVar "foo" t])
locallyReplaceSqrt t = (t, [])

replaceSqrt :: Term -> (Term, [SqrtVar])
replaceSqrt tm@(Var n)      = locallyReplaceSqrt tm
replaceSqrt tm@(Lam n ty t) = locallyReplaceSqrt tm
replaceSqrt tm@(App t1 t2)  = locallyReplaceSqrt tm
replaceSqrt tm@(Plus t1 t2) = locallyReplaceSqrt tm
replaceSqrt tm@(Constant i) = locallyReplaceSqrt tm
replaceSqrt tm@(Sqrt t)     = locallyReplaceSqrt tm 

-- foo = termSubterms locallyReplaceSqrt

--------------------------------------------------------------------------------

type SqrtWriter = Writer [SqrtVar] Term

globalSqrt :: Term -> (Term, [SqrtVar])
globalSqrt = runIdentity . runWriterT . recurse
  where
    localSqrt :: Term -> Writer [SqrtVar] Term
    localSqrt (Sqrt t) = tell [SqrtVar "*" t] >> pure (Sqrt $ Var "*")
    localSqrt t = pure t

    recurse :: Term -> Writer [SqrtVar] Term
    recurse tm = localSqrt =<<
      case tm of
        Lam n ty t -> do
          s <- localSqrt t
          pure (Lam n ty s)
        App t1 t2  -> do
          s1 <- localSqrt t1
          s2 <- localSqrt t2
          pure (App s1 s2)
        Plus t1 t2 -> do
          s1 <- localSqrt t1
          s2 <- localSqrt t2
          pure (App s1 s2)
        t -> pure t

-- globalSqrt tm@(Sqrt t)     = localSqrt tm 
-- globalSqrt tm@(Var n)      = localSqrt tm
-- globalSqrt tm@(Constant i) = localSqrt tm
-- globalSqrt tm@(Lam n ty t) = do
--   s <- localSqrt t
--   pure (Lam n ty s)
-- globalSqrt (App t1 t2) = do
--   s1 <- localSqrt t1
--   s2 <- localSqrt t2
--   pure $ Plus s1 s2
-- globalSqrt (Plus t1 t2) = do
--   s1 <- localSqrt t1
--   s2 <- localSqrt t2
--   pure $ Plus s1 s2

--------------------------------------------------------------------------------

-- local constant folding
locallyConstantFold :: Term -> Term
locallyConstantFold = \case
  Plus (Constant i1) (Constant i2) -> Constant $ i1 + i2
  t -> t

-- constant folding, normal way
constantFold :: Term -> Term
constantFold tm = locallyConstantFold $ case tm of
  Plus t1 t2 -> Plus (constantFold t1) (constantFold t2)
  Lam n ty t -> Lam n ty $ constantFold t
  App t1 t2 -> App (constantFold t1) (constantFold t2)
  x -> x

--------------------------------------------------------------------------------

-- a = traversing over this type
-- b = result type
type SimpleTraversal a b = Traversal' a b

-- traversal which accumulates the results of an effect on all subterms of a term
-- forall f. Applicative f => (Term -> f Term) -> Term -> f Term
termSubterms :: SimpleTraversal Term Term
termSubterms f = \case
  Lam n ty t -> Lam n ty <$> f t 
  App t1 t2  -> App  <$> f t1 <*> f t2
  Plus t1 t2 -> Plus <$> f t1 <*> f t2
  -- Terms without subterms. Note that you should *not* put 'f x' as the 
  -- RHS: that would say that the term was its own subterm!
  x -> pure x

-- Traversal s t a b = 
-- forall f. Applicative f =>
--   (a -> f b) -> s -> f t

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- lens    traverse
-- a    =   a        type of values in the original structure
-- b    =   b        type which values are mapped to in the new structure
-- s    =   t a      the input container type, (whose values may have type a)
-- t    =   t b      the output container type, (whose values may have type b)

-- traversal which accumulates the results of an effect on all subterms of a term
-- forall f. Applicative f => (Term -> f Term) -> Term -> f Term
termSubterms2 :: Traversal Term Term Term Term
termSubterms2 f = \case
  Lam n ty t -> Lam n ty <$> f t 
  App t1 t2  -> App  <$> f t1 <*> f t2
  Plus t1 t2 -> Plus <$> f t1 <*> f t2
  -- Terms without subterms. Note that you should *not* put 'f x' as the 
  -- RHS: that would say that the term was its own subterm!
  x -> pure x

lensConstantFold :: Term -> Term
lensConstantFold t = locallyConstantFold $ over termSubterms lensConstantFold t

-- replaceSqrt = transformOf termSubterms locallyReplaceSqrt