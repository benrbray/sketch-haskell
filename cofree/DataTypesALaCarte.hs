-- Wouter Swierstra 2008, "Data Types a la Carte"
-- https://www.cambridge.org/core/services/aop-cambridge-core/content/view/14416CB20C4637164EA9F77097909409/S0956796808006758a.pdf/data-types-a-la-carte.pdf

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

--{-# LANGUAGE OverlappingInstances #-}
module DataTypesALaCarte where

import Prelude hiding ((<*>))

--------------------------------------------------------------------------------

-- mu/fix
newtype Fix f = Fix { unfix :: f (Fix f) }

-- assuming f is a functor, then
--   Fix :: f (Fix f) -> Fix f
-- is an algebra carried by Fix f

-- in fact, by Adamek's theorem, it is the
-- initial algebra of the functor f

--------------------------------------------------------------------------------

--define a language with only integers
newtype Val e = Val Int
type IntExpr = Fix Val

-- since Val does not use its type argument, the only valid
-- expressions have the form Fix (Val x) for some integer x
exampleIntExpr :: IntExpr
exampleIntExpr = Fix (Val 5)

--------------------------------------------------------------------------------

-- define a language with only addition
data Add e = Add e e
type AddExpr = Fix Add

-- since Add has no nullary constructors, the only valid
-- expressions are infinite trees of additions
exampleAddExpr :: AddExpr
exampleAddExpr =
  Fix $ Add
    exampleAddExpr
    (Fix $ Add
      exampleAddExpr
      exampleAddExpr)

--------------------------------------------------------------------------------

-- the key idea is to combine expressions
-- by taking the coproduct of their signatures

data (f :+: g) e = InL (f e) | InR (g e)

-- writing these expressions by hand is cumbersome,
-- but we will make improvements later
addValExample :: Fix (Val :+: Add)
addValExample = Fix $ InR $ Add x y
  where x = Fix $ InL $ Val 118
        y = Fix $ InL $ Val 1219

--------------------------------------------------------------------------------

-- functor instances for terms

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap :: (a -> b) -> (f :+: g) a -> (f :+: g) b
  fmap f (InL x) = InL $ fmap f x
  fmap f (InR y) = InR $ fmap f y

--------------------------------------------------------------------------------

-- expressions with base functor f, which evaluate to type a
class Functor f => Eval f v where
  eval :: f v -> v

-- evaluation is defined separately for each term
-- term is evaluated only when its arguments have been reduced to a value

instance Eval Val Int where
  eval (Val x) = x

instance Eval Add Int where
  eval (Add x y) = x + y

-- these Eval instances compose to form an Eval instance for coproducts of terms
instance (Eval f v, Eval g v) => Eval (f :+: g) v where
  eval :: (f :+: g) v -> v
  eval (InL x) = eval x
  eval (InR y) = eval y

-- for example, addition and values together
evalAddVal :: (Add :+: Val) Int -> Int
evalAddVal = eval

--------------------------------------------------------------------------------

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unfix

evalAddValFix :: Fix (Add :+: Val) -> Int
evalAddValFix = cata eval

-- type infernce takes care of everything for us
x :: Int
x = cata eval addValExample

--------------------------------------------------------------------------------

-- injections from sub to sup
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

-- the following instance derivations will succeed in building an
-- injection for most coproducts that the user is likely to define,
-- although does run into trouble with nested instances

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj

--------------------------------------------------------------------------------

-- these injections allow us to define the following smart constructors

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = Fix . inj

val :: (Val :<: f) => Int -> Fix f
val x = inject (Val x)

(<+>) :: (Add :<: f) => Fix f -> Fix f -> Fix f
x <+> y = inject (Add x y)
infixl 6 <+>

-- it is now easier to construct and evaluate expressions
y :: Fix (Add :+: Val)
y = val 200 <+> val 5

resultY :: Int
resultY = cata eval y

--------------------------------------------------------------------------------

-- we can very easily add a new variety of term to our language

data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul Int where
  eval (Mul x y) = x * y

(<*>) :: (Mul :<: f) => Fix f -> Fix f -> Fix f
x <*> y = inject (Mul x y)
infixl 7 <*>

z :: Fix (Val :+: Add :+: Mul)
z = Fix (InL $ InL two)
    where two = Val 2

resultZ :: Int
resultZ = cata eval z

--------------------------------------------------------------------------------

data Cofree f a = a :< f (Cofree f a)
