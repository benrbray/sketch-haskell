{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yoneda where

-- fullwidth semicolon for "then" (sorry!)
(；) :: (a -> b) -> (b -> c) -> a -> c
(；) f g = g . f
{-# INLINE (；) #-}

--------------------------------------------------------------------------------

-- forward mapping
phi :: (Functor f) => forall x. (forall y. (x -> y) -> f y) -> f x
phi eta = eta id

-- reverse mapping
ihp :: (Functor f) => forall x. f x -> (forall y. (x -> y) -> f y)
ihp fx = (`fmap` fx)

-- TODO phi . ihp = id
-- TODO ihp . phi = id

--------------------------------------------------------------------------------

-- Greg Berns, "Introduction to Yoneda and Coyoneda"
-- https://gist.github.com/gregberns/ede18190d5117eea6fb51815e2eab9b2

-- f should be a functor
-- x is any concrete type
newtype Yoneda f x = Yoneda { runYoneda :: forall b. (x -> b) -> f b }

-- `Yoneda f x` represents the LHS of the Yoneda Lemma,
-- which states that `Yoneda f x` is isomorphic to `f x`

-- the isomorphism is witnessed by the following functions
toYoneda :: Functor f => f x -> Yoneda f x
toYoneda a = Yoneda (`fmap` a)

fromYoneda :: Yoneda f x -> f x
fromYoneda y = (runYoneda y) id


-- -- we can define a functor instance, allowing us to fmap over x
-- instance Functor (Yoneda f) where
--   fmap :: forall x y. (x -> y) -> Yoneda f x -> Yoneda f y
--   fmap f p = Yoneda impl
--     where impl :: forall b. (y -> b) -> f b
--           impl = (f ；) ； (runYoneda p)
-- why is this needed?
