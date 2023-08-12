-- https://crypto.stanford.edu/~blynn/compiler/fol.html

{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE CPP #-}

module FirstOrderLogic (
  runFOL,
  Term(..), Quantifier(..), Formula(..), FOL(..),
  pattern Atom, pattern Top, pattern Bot, pattern Not, pattern Quant,
  pattern (:/\), pattern (:\/), pattern (:==>), pattern (:<=>)
) where

import Data.Foldable
import Data.Char (isAlphaNum)
import Data.List (delete, union, partition, find, maximumBy, intercalate, unfoldr)
import Data.Maybe (fromMaybe)

------------------------------------------------------------

data Term
  = Var String
  | Fun String [Term]
  deriving (Eq,Ord)

data Quantifier
  = Forall
  | Exists
  deriving (Eq, Ord)

data Formula a
  = FTop                        -- true
  | FBot                        -- false
  | FAtom String [Term]         -- atomic proposition
  | FNot a                      -- negation
  | FAnd a a                    -- and
  | FOr a a                     -- or
  | FImpl a a                   -- implication
  | FIff a a                    -- if and only if
  | FQuant Quantifier String a  -- forall / exists
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- first-order logical formula
newtype FOL
  = FOL (Formula FOL)
  deriving (Eq, Ord)

-- pattern synonyms
pattern Atom s ts = FOL (FAtom s ts)
pattern Top = FOL FTop
pattern Bot = FOL FBot
pattern Not p = FOL (FNot p)
pattern p :/\ q = FOL (FAnd p q)
pattern p :\/ q = FOL (FOr p q)
pattern p :==> q = FOL (FImpl p q)
pattern p :<=> q = FOL (FIff p q)
pattern Quant q x p = FOL (FQuant q x p)

------------------------------------------------------------

instance Show Term where
  showsPrec d = \case
    Var s -> (s <>)
    Fun s xs -> (s <>) . showParen (not $ null xs) (showArgs xs)
    where
    showArgs = \case
      [] -> id
      (x:xt) -> showsPrec d x . showArgs' xt
    showArgs' xt = foldr (\f g -> (", " <>) . f . g) id $ showsPrec d <$> xt

instance Show FOL where
  showsPrec d = \case
    Atom s ts
      | not (isAlphaNum $ head s), [p, q] <- ts -> showsPrec d p . ("<=" <>) . showsPrec d q
      | otherwise -> (s <>) . showParen (not $ null ts) ((<>) $ intercalate ", " $ show <$> ts)
    Top -> ('\8868':)
    Bot -> ('\8869':)
    Not p -> ('\172':) . showsPrec 6 p
    p :/\ q -> showParen (d > 5) $ showsPrec 5 p . (" \8743 " ++ ) . showsPrec 5 q
    p :\/ q -> showParen (d > 4) $ showsPrec 4 p . (" \8744 " ++) . showsPrec 4 q
    p :==> q -> showParen (d > 3) $ showsPrec 4 p . ("\8658" ++) . showsPrec 3 q
    p :<=> q -> showParen (d > 2) $ showsPrec 3 p . ("\8660" ++) . showsPrec 3 q
    Quant q x p -> showParen (d > 0) $ (charQ q ++) . (x <>) . (". " ++) . showsPrec 0 p
      where charQ Forall = "\8704 "
            charQ Exists = "\8707 "

---- recursion helpers -------------------------------------

bifix :: (a -> b) -> (b -> a) -> a
bifix f g = g $ f $ bifix f g

ffix :: Functor f => ((f a -> f b) -> a -> b) -> a -> b
ffix = bifix fmap

unmap :: (Formula FOL -> Formula FOL) -> FOL -> FOL
unmap h (FOL t) = FOL (h t)

------------------------------------------------------------

runFOL :: FOL -> IO ()
runFOL fol = do
  print fol
  print $ simplify fol
  print $ nnf fol

---- free variables ----------------------------------------

freeVarsTerm :: Term -> [String]
freeVarsTerm = \case
  Var x -> [x]
  Fun _ xs -> foldr union [] $ freeVarsTerm <$> xs

freeVars :: FOL -> [String]
freeVars = ffix \h -> \case
  Atom _ ts -> foldr union [] $ freeVarsTerm <$> ts
  Quant _ x p -> delete x $ freeVars p
  FOL t -> foldr union [] $ h t

simplify :: FOL -> FOL
simplify = ffix \h fol ->
  case unmap h fol of
    Not (Bot)    -> Top
    Not (Top)    -> Bot
    Not (Not p)  -> p
    Top :\/ _    -> Top
    Bot :/\ _    -> Bot
    _ :\/ Top    -> Top
    _ :/\ Bot    -> Bot
    Top :/\ p    -> p
    Bot :\/ p    -> p
    p :/\ Top    -> p
    p :\/ Bot    -> p
    _ :==> Top   -> Top
    Bot :==> _   -> Top
    Top :==> p   -> p
    p :==> Bot   -> Not p
    p :<=> Top   -> p
    Top :<=> p   -> p
    Bot :<=> Bot -> Top
    p :<=> Bot   -> Not p
    Bot :<=> p   -> Not p
    Quant _ x p | x `notElem` freeVars p -> p
    t -> t

-- negation normal form
nnf :: FOL -> FOL
nnf = ffix \h -> unmap h . \case
  Not (Not p)            -> p
  Not (p :\/ q)          -> Not p :/\ Not q
  Not (p :/\ q)          -> Not p :\/ Not q
  p :==> q               -> Not p :\/ q
  p :<=> q               -> (Not p :\/ q) :/\ (p :\/ Not q)
  Not (Quant Forall x p) -> Quant Exists x (Not p)
  Not (Quant Exists x p) -> Quant Forall x (Not p)
  t -> t

substTerm :: (String -> Maybe Term) -> Term -> Term
substTerm f t = case t of
  Var x -> fromMaybe t $ f x
  Fun s as -> Fun s $ substTerm f <$> as

subst :: (String -> Maybe Term) -> FOL -> FOL
subst f = ffix \h -> \case
  Atom s ts -> Atom s $ substTerm f <$> ts
  t -> unmap h t

