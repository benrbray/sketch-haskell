{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Parser (
  folFormula,
  mustFOL
) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import FirstOrderLogic

folFormula :: Parsec () String FOL
folFormula = space *> iff where
  iff = foldr1 (:<=>) <$> sepBy1 impl (want "<=>")
  impl = foldr1 (:==>) <$> sepBy1 disj (want "==>")
  disj = foldl1 (:\/) <$> sepBy1 conj (want "\\/" <|> want "|")
  conj = foldl1 (:/\) <$> sepBy1 nope (want "/\\" <|> want "&")
  nope = do
    fs <- many (const Not <$> (want "~" <|> want "!"))
    t <- vip
    pure $ foldr (\_ x -> Not x) t fs
  vip = between (want "(") (want ")") folFormula
    <|> const Top <$> want "1"
    <|> const Bot <$> want "0"
    <|> qua
    <|> atom
  qua = do
    f <- Quant <$> (want "forall" *> pure Forall
             <|>  want "exists" *> pure Exists)
    vs <- some var
    fo <- want "." *> folFormula
    pure $ foldr f fo vs
  atom = (bin =<< Var <$> var) <|> do
    (f, xs) <- call
    option (Atom f xs) $ bin (Fun f xs)
  call = (,) <$> con <*> option [] (between (want "(") (want ")")
    $ sepBy term $ want ",")
  bin x = do
    want "<="
    y <- term
    pure $ Atom "<=" [x, y]
  term = Var <$> var <|> uncurry Fun <$> call
  var = (:) <$> lowerChar <*> (many alphaNumChar <* space)
  con = (:) <$> upperChar <*> (many alphaNumChar <* space)
  want :: String -> Parsec () String ()
  want s = try $ do
    s' <- (some (oneOf "<=>\\/&|~!")
      <|> some alphaNumChar
      <|> ((:[]) <$> oneOf "(),.")) <* space
    if s == s' then pure () else fail $ "want " <> s

mustFOL :: String -> FOL
mustFOL = either (error . show) id . parse folFormula ""