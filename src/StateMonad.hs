module StateMonad where

import Control.Monad

-- f then g
f >>> g = g . f

-- runState :: State s a -> s -> (a, s)

-- fmap over first argument of a tuple
first :: (a1 -> a2) -> (a1, b) -> (a2, b)
first f (x, y) = (f x, y)

-- an instance of @State s a@ is an action which
--   produces a result of type @a@
--   along with a transition from input to output states, of type @s@
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  -- map over the "result" of the stateful action
  fmap :: (a -> b) -> State s a -> State s b
  fmap f = runState >>> (>>> first f) >>> State

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)

  -- derived using our Monad instance below
  (<*>) = ap

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State runA) f = State (runF . runA)
    where runF = uncurry $ runState . f

