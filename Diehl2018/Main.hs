module Main where

import Prelude hiding (succ)
import Data.Text (Text)
import Data.Text qualified as T
import HindleyMilner
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Gen

--------------------------------------------------------------------------------

nl :: Text -> Text -> Text
t1 `nl` t2 = t1 <> "\n" <> t2
(<.>) :: Text -> Text -> Text
t1 <.> t2 = t1 <> " " <> t2

class Pretty a where
  pretty :: a -> Text

  prettyPrint :: a -> IO ()
  prettyPrint = putStrLn . T.unpack . pretty

instance Pretty a => Pretty [a] where
  pretty :: [a] -> Text
  pretty = foldr ((<>) . pretty) ""

instance Pretty Type where
  pretty :: Type -> Text
  pretty (TypeVar t) = pretty t
  pretty (TypeArr t1 t2) = "(" <> pretty t1 <> " --> " <> pretty t2 <> ")"
  pretty t = (T.pack . show) t

instance Pretty TV where
  pretty :: TV -> Text
  pretty (TV (Name t)) = t
  pretty (TV (Fresh k)) = "#" <> T.pack (show k)

instance Pretty Constraint where
  pretty :: Constraint -> Text
  pretty (Constraint t1 t2) =
    pretty t1 <.> pretty t2

instance Pretty TypeError where
  pretty :: TypeError -> Text
  pretty (UnificationFail t1 t2) = "cannot unify" `nl` pretty t1 `nl` pretty t2
  pretty (InfiniteType x t) = "infinite type" `nl` pretty x `nl` pretty t
  pretty err = T.pack . show $ err

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let tvX = TypeVar (TV $ Name "X")
  let tvY = TypeVar (TV $ Name "Y")
  let tvZ = TypeVar (TV $ Name "Z")

  let x = Name "X"
  let y = Name "Y"
  let z = Name "Z"
  let vx = Var x
  let vy = Var y
  let vz = Var z

  let plus1 = Lam x (Op Add vx (Lit (LInt 1)))
  let constFn = Lam x (Lam y vx)
  let example1 = Lam x (Lam y (Lam z (Op Add vx (Op Add vy vz))))

  let app = Lam x (Lam y (App vx vy))
      example2 = Let x plus1 vx

  let result = hindleyMilner example2
  case result of
    Left err  -> prettyPrint err
    Right tpe -> do
      prettyPrint tpe

  pure ()

hindleyMilner :: Expr -> Either TypeError Type
hindleyMilner e = do
  (partialType, constrs) <- runInfer $ infer e
  subst <- runSolve $ solve (emptySubst, constrs)
  pure $ applySubst subst partialType
