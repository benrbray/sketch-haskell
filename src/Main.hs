{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Text
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Typeable
import Control.Monad.Trans.Maybe

------------------------------------------------------------

main :: IO ()
main = print "Hello"

data CardType = Visa | AmEx | MasterCard

-- kind (* -> *) -> *
data FormTemplate f = FormTemplate {
  _id         :: f Int,
  _email      :: f Text,
  _cardType   :: f CardType,
  _cardNumber :: f Text
}

type Record t = t Identity  -- ((* -> *) -> *) -> *
type Partial t = t Maybe    -- ((* -> *) -> *) -> *

type FilledForm = Record FormTemplate     -- kind *
type DraftForm  = Partial FormTemplate    -- kind *

------------------------------------------------------------

infixr 0 ~>
-- natural transformations between functors f and g
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- natural transformation with a newtype wrapper
newtype f :~> g = Natural { ($$) :: f ~> g }
  deriving (Typeable)

------------------------------------------------------------

-- f :: (* -> *) -> *
class FunctorK f where
  ffmap :: (Functor g, Functor h) => (g ~> h) -> f g -> f h

instance FunctorK FormTemplate where
  ffmap :: (Functor g, Functor h) => (g ~> h) -> FormTemplate g -> FormTemplate h
  ffmap eta (FormTemplate id email cardType number)
    = FormTemplate (eta id) (eta email) (eta cardType) (eta number)

maybeify :: Identity a -> Maybe a
maybeify = Just . runIdentity 

makePartial :: FunctorK t => Record t -> Partial t
makePartial = ffmap maybeify

------------------------------------------------------------

-- (* -> *) -> (* -> *) -> *
newtype ReaderK g h = ReaderK { runReaderK :: g ~> h }

-- (ReaderK g) is equivalent to (g ~>)
--   which is just a function that knows how to read values from a g-context

-- ReaderK is a functor in the sense that
--   if we have a function that reads from a g-context to produce values in a u-context
--   and we have a way of converting u-contexts to v-contexts
--   we can make a function capable of reading values from h-contexts

instance Functor g => FunctorK (ReaderK g) where
  ffmap :: (Functor u, Functor v) => (u ~> v) -> ReaderK g u -> ReaderK g v
  ffmap eta (ReaderK f) = ReaderK (eta . f)

------------------------------------------------------------

-- r :: (* -> *) -> *
class FunctorK r => RepresentableK r where
  type RepK r :: * -> *        -- representation (in theory a functor, but in practice a GADT) 
  fwd :: forall f. (RepK r ~> f) -> r f  -- tabulate
  bck :: forall f. r f -> (RepK r ~> f)  -- index

-- bck :: forall f. r f -> (forall b. R b -> f b)

-- say R = RepK r :: * -> *
-- need
-- f (r f) -> (R ~> f)
-- f (r f) -> (forall a. R a -> f a)


-- bind :: forall a b.  f a -> (a -> f b) -> f b
--                      f (r f) -> (r f -> f b) -> f b

-- bind frf :: forall b. (r f -> f b) -> f b

-- translated from tofu.higherKind.Embed.scala
-- u :: (* -> *) -> *
-- f :: (* -> *)
class Embed u where
  embed :: (Monad f) => f (u f) -> u f

-- instance RepresentableK r => (Embed r) where
--   -- bind :: (a -> f b) -> f a -> f b
--   --         ( (r f) -> f b) -> f (r f) -> f b
--   --         ( (r f) -> f (r f) ) -> f (r f) -> f (r f)
--   embed :: (Monad f) => f (r f) -> r f
--   embed frf = fwd _
--               where 
--                 --go :: forall a. (r f -> f a) -> f a
--                 go = (>>=) frf

------------------------------------------------------------

newtype EbyamT a m = Wrap (MaybeT m a)

-- instance Embed (EbyamT m) where
--   embed :: (Monad f) => f (EbyamT m f) -> EbyamT m f
--   embed ft = Wrap (go _)
--              where
--                go = (>>=) ft


------------------------------------------------------------

newtype ReaderT a b = MkReaderT { runReaderT :: a ~> b }

-- instance RepresentableK (ReaderT a) where
--   type RepK (ReaderT a) = a
--   fwd :: forall b. (a ~> b) -> ReaderT a b
--   fwd = MkReaderT

--   bck :: forall b. ReaderT a b -> (a ~> b)
--   bck = runReaderT

-- instance RepresentableK ((:~>) a) where
--   type RepK ((:~>) a) = a
--   fwd :: forall b. (a ~> b) -> (a :~> b)
--   fwd = Natural

--   bck :: forall b. (a :~> b) -> (a ~> b)
--   bck = ($$)

------------------------------------------------------------

-- representative
-- (will typically be a GADT -- describes the value we expect to find at a given position in a record)
-- phantom type why?
data FormRep a where
    Id :: FormRep Int
    Email :: FormRep Text
    CardType :: FormRep CardType
    CardNumber :: FormRep Text

instance RepresentableK FormTemplate where
    type RepK FormTemplate = FormRep

    fwd :: (FormRep ~> f) -> FormTemplate f
    fwd eta = FormTemplate (eta Id) (eta Email) (eta CardType) (eta CardNumber)

    bck :: FormTemplate f -> (FormRep ~> f)
    bck p Id         = _id p
    bck p Email      = _email p
    bck p CardType   = _cardType p
    bck p CardNumber = _cardNumber p

-- yoneda
formYoneda :: FormTemplate FormRep
formYoneda = fwd id