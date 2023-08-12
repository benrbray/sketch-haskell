module Main where

import FirstOrderLogic (runFOL, FOL)
import System.Environment (getArgs)
import qualified Data.Text as T
import Parser (mustFOL)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-f", fp] -> withParsedFile fp runFOL
    _ -> putStrLn "USAGE: fol -f <filename>"

withParsedFile :: FilePath -> (FOL -> IO ()) -> IO ()
withParsedFile fp h = do
  source <- readFile fp
  let p = mustFOL source
  h p
  -- case p_ of
  --   Left err -> say (dispParseErr err)
  --   Right p -> h p