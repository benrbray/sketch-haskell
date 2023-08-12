module Profunctor where

-- class Profunctor p where
--   -- contravariant in first, covariant in second
--   dimap :: (a1 -> a2) -> (b1 -> b2) -> p a2 b1 -> p a1 b2 
--   -- contravariant in first argument
--   lmap :: (a1 -> a2) -> h a2 b -> h a1 b
--   lmap f = (`dimap` filter)
--   -- covariant in second argument
--   rmap :: (b1 -> b2) -> p a b1 -> p a b2
--   rmap = dimap id

-- -- http://comonad.com/reader/2008/kan-extension-iii/
-- type Dinatural f g = forall a. f a a -> g a a

------------------------------------------------------------

-- diagonal :: Profunctor p => (a -> b) -> p a a -> (p a b, p b a) 
-- diagonal paa = 