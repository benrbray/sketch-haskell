module Archived.Fresh where

-- newtype Fresh v a = Fresh (State v a)

-- runFresh :: Fresh v a -> v -> (a, v)
-- runFresh (Fresh s) = runState s

-- class Succ a where
--   -- generates a unique successor for each value
--   succ :: a -> a

-- instance Succ Int where
--   succ x = x + 1

-- instance Succ Text where
--   succ s =
--     case T.splitOn "#" s of
--       [s0] -> s0 <> "#0"
--       [s0,k0] -> case T.decimal k0 of
--         Right (k1 :: Int,"") -> s0 <> "#" <> T.pack (show (k1 + 1))
--         _                    -> error $ "could not parse integer from " ++ show s
--       _ -> error $ "invalid variable name " ++ show s

-- instance Succ v => Functor (Fresh v) where
--   fmap :: (a -> b) -> Fresh v a -> Fresh v b
--   fmap f (Fresh x) = Fresh $ fmap f x

-- instance Succ v => Applicative (Fresh v) where
--   pure :: a -> Fresh v a
--   pure a = Fresh $ pure a

--   (<*>) :: Fresh v (a -> b) -> Fresh v a -> Fresh v b
--   (Fresh f) <*> (Fresh x) = Fresh $ f <*> x

-- instance Succ v => Monad (Fresh v) where
--   (>>=) :: Fresh v a -> (a -> Fresh v b) -> Fresh v b
--   x >>= f
--     = Fresh . state $ \v0 ->
--         let (a,v1) = runFresh x v0
--         in runFresh (f a) v1