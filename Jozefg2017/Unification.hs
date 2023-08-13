{-# LANGUAGE MonadComprehensions #-}

-- jozefg, "Higher Order Unification"
-- A simple, concise implementation of Huet's Algorithm
-- https://github.com/jozefg/higher-order-unification/blob/21382f44205aa3d8b115fe2b2eba47489da4b492/explanation.md

module Unification where

import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.List (foldl')
import Control.Monad.Logic
import Control.Monad.Gen
import Control.Monad (guard, replicateM)
import Data.Foldable (fold)
import Control.Monad.Trans.Class (lift)
import Protolude (applyN)
import Control.Monad (foldM)
import Control.Monad (join)
import Control.Monad ((<=<))
import Control.Monad ((>=>))

type T = LogicT

-- locally nameless approach represents
--   bound variables with de Bruijn indices
--   free variables with globally unique identifiers

type Name = Int
type Idx = Int

data Term
  = Uni           -- universe
  | FreeVar Name  -- free variable, uniquely named
  | MetaVar Name  -- unification variable, uniquely named
  | BoundVar Idx  -- bound variable, de Bruijn indexed
  | App Term Term -- application
  | Lam Term      -- lambda binding
  | Pi Term Term  -- dependent function type (first arg is a type, second is the body)
  deriving (Eq, Show, Ord)

-- adds (+i) to all bound variables in a term
raise :: Int -> Term -> Term
raise = go 0
  where
    -- adds `k` to all indices larger than `lower`
    go :: Int -> Int -> Term -> Term
    go lower k t =
      case t of
        Uni        -> Uni
        FreeVar n  -> FreeVar n
        MetaVar m  -> MetaVar m
        BoundVar i -> if i > lower
                        then BoundVar (i + k)
                        else BoundVar i
        App l r    -> App (go lower k l) (go lower k r)
        Lam body   -> Lam (go (lower + 1) k body)
        Pi tp body -> Pi (go lower k tp) (go (lower + 1) k body)

--------------------------------------------------------------------------------

-- substitution of a single bound variable
substBound :: Term -> Int -> Term -> Term
substBound new i t = case t of
  Uni -> Uni
  FreeVar n  -> FreeVar n
  MetaVar m  -> MetaVar m
  BoundVar j -> case compare j i of
                  LT -> BoundVar j
                  EQ -> new
                  GT -> BoundVar (j - 1)
  App l r    -> App (substBound new i l) (substBound new i r)
  Lam body   -> Lam (substBound (raise 1 new) (i + 1) body)
  Pi tp body -> Pi (substBound new i tp) (substBound (raise 1 new) (i + 1) body)

-- substitution of a single metavariable
substMeta :: Term -> Name -> Term -> Term
substMeta new m t = case t of
  Uni        -> Uni
  FreeVar n  -> FreeVar n
  MetaVar m  -> new
  BoundVar j -> BoundVar j
  App l r    -> App (substMeta new m l) (substMeta new m r)
  Lam body   -> Lam (substMeta (raise 1 new) m body)
  Pi tp body -> Pi (substMeta new m tp) (substMeta (raise 1 new) m body)

type Subst = Map Name Term

-- substitute several metavariables simultaneously
substMetaMany :: Subst -> Term -> Term
substMetaMany = flip $ M.foldrWithKey (flip substMeta)

-- combine two disjoint substitutions
(<+>) :: Subst -> Subst -> Subst
s1 <+> s2 | not (M.null (M.intersection s1 s2)) = error "cannot merge overlapping substitutions"
s1 <+> s2 = M.union (substMetaMany s1 <$> s2) s1

--------------------------------------------------------------------------------

-- beta reduction
reduce :: Term -> Term
reduce t = case t of
  Uni        -> Uni
  FreeVar n  -> FreeVar n
  MetaVar m  -> MetaVar m
  BoundVar j -> BoundVar j
  Lam body   -> Lam (reduce body)
  Pi tp body -> Pi (reduce tp) (reduce body)
  App l0 r ->
    case reduce l0 of
      Lam body -> reduce (substBound r 0 body)
      l1       -> App l1 (reduce r)

--------------------------------------------------------------------------------

-- return a set of all metavariables listed in a term
metavars :: Term -> S.Set Name
metavars t = case t of
  Uni        -> S.empty
  FreeVar _  -> S.empty
  BoundVar _ -> S.empty
  MetaVar m  -> S.singleton m
  App l r    -> metavars l <> metavars r
  Lam body   -> metavars body
  Pi tp body -> metavars tp <> metavars body

-- checks if a term is closed (no free variables)
isClosed :: Term -> Bool
isClosed t = case t of
  FreeVar _  -> False
  MetaVar _  -> True
  BoundVar _ -> True
  Uni        -> True
  App l r    -> isClosed l && isClosed r
  Lam body   -> isClosed body
  Pi tp body -> isClosed tp && isClosed body


-- Terms are "stuck" when they cannot be reduced further.  This
-- happens when the term is a metavariable or an application thereof.
isStuck :: Term -> Bool
isStuck (MetaVar _) = True
isStuck (App f _) = isStuck f
isStuck _ = False

--------------------------------------------------------------------------------

-- convert an application @f a1 a2 a3 ...@ into a
-- function @f@ with a list of arguments @[a1, a2, a3, ...]@
peelAppTelescope :: Term -> (Term, [Term])
peelAppTelescope t = go t []
  where
    go (App f r) rest = go f (r : rest)
    go t rest         = (t, rest)

-- inverse of peelAppTelescope
applyApp :: Term -> [Term] -> Term
applyApp = foldl' App

--------------------------------------------------------------------------------

-- backtracking monad which supports generation of fresh metavariable names
type UnifyM = LogicT (Gen Name)
type Constraint = (Term, Term)

-- take a constraint and produce a set of equivalent constraints
-- (for instance, @App (FreeVar 0) m1 === App (FreeVar 0) m2@ can
--  be reduced to the constraint @m1 === m2)@
simplify :: Constraint -> UnifyM (S.Set Constraint)
simplify (t1, t2)
  | t1 == t2        = pure S.empty
  -- allow two terms to unify if they share a common reduct
  | reduce t1 /= t1 = simplify (reduce t1, t2)
  | reduce t2 /= t2 = simplify (t1, reduce t2)
  -- when unifying applications of free variables (f1 a1 ... an) === (f2 b1 ... bm)
  --   arities must match (n === m)
  --   free variables must match (f1 === f2)
  --   all arguments must unify (ai === bi, for all i)
  | (FreeVar i, ctx1) <- peelAppTelescope t1,
    (FreeVar j, ctx2) <- peelAppTelescope t2 = do
      guard (i == j && length ctx1 == length ctx2)
      fold <$> mapM simplify (zip ctx1 ctx2)
  | Lam body1 <- t1,
    Lam body2 <- t2 = do
      v <- FreeVar <$> lift gen
      return $ S.singleton (substBound v 0 body1, substBound v 0 body2)
  | Pi tp1 body1 <- t1,
    Pi tp2 body2 <- t2 = do
      v <- FreeVar <$> lift gen
      return $ S.fromList
        [(substBound v 0 body1, substBound v 0 body2),
         (tp1, tp2)]
  | otherwise =
    if isStuck t1 || isStuck t2
    then return $ S.singleton (t1, t2)
    else mempty

-- | reflexive transitive closure of @simplify@
repeatedlySimplify :: S.Set Constraint -> UnifyM (S.Set Constraint)
repeatedlySimplify cs0 = do
  cs1 <- fold <$> traverse simplify (S.toList cs0)
  if cs1 == cs0 then return cs0 else repeatedlySimplify cs1

--------------------------------------------------------------------------------

-- | generate candidate solutions to constraints of the form
--     @M a1 a2 ... an = A b1 b2 ... bm@
-- where @M@ is a metavariable (flex), and
--       @A@ is some term, probably a free variable (rigid)
tryFlexRigid :: Constraint -> [UnifyM [Subst]]
tryFlexRigid (t1, t2)
  | (MetaVar m, ctx1) <- peelAppTelescope t1,
    (stuckTerm, ctx2) <- peelAppTelescope t2,
    not (m `S.member` metavars t2) =
      map (generateSubst (length ctx1) m stuckTerm) [0 ..]
  | (MetaVar m, ctx1) <- peelAppTelescope t2,
    (stuckTerm, ctx2) <- peelAppTelescope t1,
    not (m `S.member` metavars t1) =
      map (generateSubst (length ctx1) m stuckTerm) [0 ..]
  | otherwise = []

-- Let M be a metavar and A be a term.  When unifying
--      M a1 ... an =?= A b1 ... bm
-- there are only two possible forms M may take:
--      M = λ x1. ... λ xn. xi (M1 x1 ... xn) ... (Mr x1 ... xn) (i = 1..n)
--   or M = λ x1. ... λ xn. A (M1 x1 ... xn) ... (Mr x1 ... xn)  (if A is a closed term)
-- QUESTION: why?
generateSubst :: Int -> Name -> Term -> Int -> UnifyM [Subst]
generateSubst nBound metaVar f nArgs = do
  -- generate @nArgs@ fresh metavariables
  metaVars <- replicateM nArgs (MetaVar <$> lift gen)

  let -- construct the innermost @nBound@ bound variables
      boundVars = [ BoundVar j | j <- [0 .. nBound-1]]
      -- pass bound variables @(0, ..., n-1)@ as arguments to each fresh metavar
      args       = [ applyApp m boundVars | m <- metaVars]
      -- applies a given term to the list of @args@
      applyToArgs  = (`applyApp` args)
      -- helper function to wrap a term with n lambdas
      wrapLam    = applyN nBound Lam
      candidates = (applyToArgs . wrapLam) <$> (boundVars ++ [ f | isClosed f])

  pure $ M.singleton metaVar <$> candidates

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

interleaveMany :: [UnifyM a] -> UnifyM a
interleaveMany = foldr interleave mempty

-- takes the current substitution and a set of constraints
-- to produce a solution substitution and a set of flex-flex equations
unify :: Subst -> S.Set Constraint -> UnifyM (Subst, S.Set Constraint)
unify s0 cs0 = do
  -- 1. apply the given substitution to all our constraints
  let cs1 = S.map (substConstraint s0) cs0
  -- 2. simplify the set of constraints
  cs2 <- repeatedlySimplify cs1
  -- 3. separate flex-flex equations from flex-rigid ones
  let (flexFlex, flexRigid) = S.partition (\(t1, t2) -> isStuck t1 && isStuck t2) cs2

  if S.null flexRigid
    -- 4. if there are no flex-rigid equations, we are done
    then pure (s0, flexFlex)
    else do
      -- 5. pick a random flex-rigid equation and generate possible solutions
      let partialSubsts = tryFlexRigid (randomElement flexRigid)
      -- 6. try each solution and attempt to unify remaining
      --    constraints, with backtracking if we get stuck
      trySubsts2 cs2 partialSubsts

  pure (s0, cs0)
  where
    substConstraint :: Subst -> Constraint -> Constraint
    substConstraint s (t1, t2) = (substMetaMany s t1, substMetaMany s t2)

    -- note: S.findMax is an internal method that relies on implementation
    -- details of Data.Set, and should probably not be used
    randomElement :: Set a -> a
    randomElement = S.findMax

    trySubsts2 :: S.Set Constraint -> [UnifyM [Subst]] -> UnifyM (Subst, S.Set Constraint)
    trySubsts2 cs [] = mempty
    trySubsts2 cs substs = go substs
      where 
        unifyWith s = unify (s <+> s0) cs
        go :: [UnifyM [Subst]] -> UnifyM (Subst, Set Constraint)
        go = (interleaveMany . fmap3 unifyWith) >=> interleaveMany
        

    -- trySubsts :: S.Set Constraint -> [UnifyM [Subst]] -> UnifyM (Subst, S.Set Constraint)
    -- trySubsts cs [] = mempty -- given no substitutions, we have no choice but to fail
    -- trySubsts cs (mss : psubsts) = do
    --   ss <- mss

    --   -- note: interleave is essentially equivalent to @mplus@ from the list
    --   -- monad but searches fairly in the case of infinite lists. This takes
    --   -- care of handling backtracking in a seamless and mostly invisible way!
    --   let unifyWith x = unify (x <+> s0) cs
    --   let foo   = map unifyWith ss -- [ unifyWith newS | newS <- ss]
    --   let these = foldr interleave mempty foo
    --   let those = trySubsts cs psubsts
    --   these `interleave` those