-- https://byorgey.wordpress.com/2020/05/19/competitive-programming-in-haskell-sorting-tree-shapes/
{-# LANGUAGE DeriveFunctor #-}

module TreeShapes where


import Control.Arrow
import Data.List

main = interact $
  lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve = map (foldl' (flip ins) Empty >>> (() <$)) >>> sort >>> group >>> length
                                           -- or: >>> S.fromList >>> S.size

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord, Functor)

ins :: Ord a => a -> Tree a -> Tree a
ins a Empty = Node a Empty Empty
ins a (Node x l r)
  | a < x     = Node x (ins a l) r
  | otherwise = Node x l (ins a r)
