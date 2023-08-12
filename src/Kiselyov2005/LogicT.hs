-- https://okmij.org/ftp/papers/LogicT.pdf

module Kiselyov2005.LogicT where

--------------------------------------------------------------------------------

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

--------------------------------------------------------------------------------

-- the straightforward depth-first search performed by most MonadPlus impls
-- is not fair:  a nondeterministic choice between two alternatives tries every
-- solution from the first alternative before any solution from thesecond.
--
-- So, when the first alternative offers infinitely many solutions, the second
-- is never tried, making the search _incomplete_.
--
-- The second deficiency:  Existing backtracking monads adopt prolog's _cut_,
-- which confounds negation with pruning.

class LogicT t where
  msplit :: (Monad m, LogicT t, MonadPlus (t m)) => t m a -> t m (Maybe (a, t m a))