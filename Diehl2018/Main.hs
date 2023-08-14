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
  let tvX = TypeVar (TV "X")
  let tvY = TypeVar (TV "Y")
  let tvZ = TypeVar (TV "Z")

  let t1 = tvX
  let t2 = TypeArr tvX tvZ

  let result = runGen $ runExceptT (unify t1 t2)
  case result of
    Right x -> print x
    Left err -> prettyPrint err
  pure ()