-- William Yao, 2022, "Why Continuation-Passing Style Works, and the Cont Monad"
-- https://www.williamyaoh.com/posts/2022-05-02-the-cont-monad.html

module Yao2022 where

import Protolude
import GHC.Err (error)
import Prelude (id)

--------------------------------------------------------------------------------

-- | embodies the Yoneda Lemma isomorphism between @a@ and @(a -> r) -> r@
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- A @Cont r a@ is just an "a" waiting to be fed as input to a function

-- | call with concurrent continuation
-- (a -> ())
callCC :: forall a b r. ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f =
  Cont $ \cc ->
    let skip x = Cont (const (cc x))
    in runCont (f skip) cc

foo :: Cont (Cont r a) (a -> Cont r b)
foo = Cont callCC

--------------------------------------------------------------------------------

factCPS :: Int -> (Int -> r) -> r
factCPS = error ""

fivebangbang :: Int
fivebangbang
  = factCPS 5 $ \f5 ->
      factCPS f5 id

-- computes 3! + 5! + 7!
sumFact357 :: (Int -> r) -> r
sumFact357 cc =
  factCPS 3 $ \f3 ->
    factCPS 5 $ \f5 ->
      factCPS 7 $ \f7 ->
        cc (f3 + f5 + f7)

--------------------------------------------------------------------------------

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont g) = Cont $ \cc -> g (cc . f)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont (\f -> f x)

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (Cont f) <*> (Cont x) = Cont (\cc -> f (\g -> x (cc . g)))

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (Cont ma) >>= f
    = Cont (\cc -> ma $ \x -> runCont (f x) cc)

--------------------------------------------------------------------------------

bar :: Bool -> Text -> Cont r Text
bar flag name =
  callCC (\cc -> do
    msg <- addGreeting flag name cc
    pure ("Result: " <> msg))

addGreeting :: Bool -> Text -> (Text -> Cont r Text) -> Cont r Text
addGreeting flag name k = do
  if flag then k "flag set, exiting early"
  else pure ("Hello, " <> name)