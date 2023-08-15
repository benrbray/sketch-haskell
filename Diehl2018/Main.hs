module Main where

import Prelude hiding (succ)
import Data.Text
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
  prettyPrint = putStrLn . unpack . pretty

instance Pretty Type where
  pretty :: Type -> Text
  pretty = pack . show

instance Pretty TV where
  pretty :: TV -> Text
  pretty = pack . show

instance Pretty TypeError where
  pretty :: TypeError -> Text
  pretty (CannotUnify t1 t2) = "cannot unify" `nl` pretty t1 `nl` pretty t2
  pretty (InfiniteType x t) = "infinite type" `nl` pretty x `nl` pretty t
  pretty err = pack . show $ err

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

  let example1 = Lam x (Lam y (Lam z (Op Add vx (Op Add vy vz))))

  let result = runInfer $ infer emptyTypeEnv example1
  case result of
    Left err  -> prettyPrint err
    Right (subst, tpe) -> do
      print subst
      prettyPrint tpe

  pure ()