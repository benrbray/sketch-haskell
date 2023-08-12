{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
module RecursionSchemes where

import Control.Arrow

-- Thomson 2014, "Recursion Schemes"
-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html

data ExprF a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

------------------------------------------------------------

newtype Mu f = In { out :: f (Mu f) }

type Expr = Mu ExprF


-- In :: ExprF Expr -> Expr
-- out :: Expr -> ExprF Expr

-- f-algebra with carrier a
type Algebra f a = f a -> a

x :: Algebra f (Mu f)
x = In

------------------------------------------------------------

ten :: Expr
ten = In ( Literal { intVal = 10 } )

add :: Expr
add = In ( Ident { name = "add" } )

call :: Expr
call = In ( Call { func = add, args = [ten,ten] } )

--------------------

-- cata = "downwards"
-- suitable for bottom-up recursion with suitably "local" computations
-- "histomorphisms" are like catamorphisms that allow acting upon past history
cata :: Functor f => (f b -> b) -> Mu f -> b
cata fn =
  out                    -- peel off one layer of the Expr
  >>> fmap (cata fn)  -- apply `mystery fn` to every subexpr
  >>> fn                 -- apply fn to the top-level

-- Q? when does cata know to stop recursion?
-- A: `cata fn` is the identity over the inhabitants of `f Void`, in this case Literal and Ident

cataExpr :: (ExprF b -> b) -> Expr -> b
cataExpr = cata @ExprF

-- ExprF Int -> Int
countNodes :: Algebra ExprF Int
countNodes (Literal _) = 1
countNodes (Ident _) = 1
countNodes (Index target idx) = target + idx + 1
countNodes (Unary _ arg) = 1 + arg
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args) = fn + sum args + 1
countNodes (Paren arg) = arg + 1

count :: Expr -> Int
count = cataExpr countNodes

-- laws
-- cata In = id
-- cata (alg >>> fmap func) = (cata alg) >>> func (avoid successive invocations of fmap)
-- cata (f >>> In) >>> cata g = cata (f >>> g)
