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
module NestedType where

------------------------------------------------------------

type f ~> g = forall x. f x -> g x

class HFunctor f where
  -- maps functors to functors
  ffmap :: Functor g => (a -> b) -> f g a -> f g b
  -- maps natural transformations to natural transformations
  hfmap :: (g ~> h) -> (f g ~> f h)


-- fixpoint
newtype Mu f a = In { unIn :: f (Mu f) a }

------------------------------------------------------------

-- perfect trees datatype
data PTree a = PLeaf a | PNode (PTree (a,a))

-- perfect trees functor
data HPTree f a = HPLeaf a | HPNode (f (a,a))

-- PTree is equivalent to
type MuPTree a = Mu HPTree a