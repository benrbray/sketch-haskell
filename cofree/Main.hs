module Main where

import Data.Function

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "hello!"
  pure ()

--------------------------------------------------------------------------------

class Functor w => Comonad w where
  extract   :: w a -> a               -- produce a value from context
  duplicate :: w a -> w (w a)         -- add an additional layer of context
  extend :: (w a -> b) -> w a -> w b  -- extend a local op to a global op by applying at all contexts

  extend f = fmap f . duplicate
  duplicate = extend id

  -- extend extract      = id
  -- extract . extend f  = f
  -- extend f . extend g = extend (f . extend g)

-- default implementation of extend
-- duplicate :: w a -> w (w a)
-- f         :: w a -> b
-- fmap      :: (c -> d) -> w c -> w d
-- fmap f    :: w (w a) -> w b
-- fmap f . duplicate :: w a -> w b

--------------------------------------------------------------------------------

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f = Identity . f . runIdentity

instance Comonad Identity where
  extract :: Identity a -> a
  extract = runIdentity

  duplicate :: Identity a -> Identity (Identity a)
  duplicate = Identity

  extend :: (Identity a -> b) -> Identity a -> Identity b
  extend f = Identity . f

--------------------------------------------------------------------------------

newtype Traced m a = Traced { runTraced :: m -> a }

instance Monoid m => Comonad ((->) m) where
  extract :: (m -> a) -> a
  extract f = f mempty

  duplicate :: (m -> a) -> (m -> m -> a)
  duplicate f m1 m2 = f (m1 <> m2)

--------------------------------------------------------------------------------

