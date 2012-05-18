{-# LANGUAGE
      NoMonomorphismRestriction,
      TypeSynonymInstances #-}

-- | Variational math expressions.
module Examples.MathExpressions where

import Data.Variational

--
-- * Object language
--

-- | Simple math expressions in the tagless-final style.
class Exp a where
  lit :: Int -> a
  neg :: a -> a
  add :: a -> a -> a

-- | Evaluator.
instance Exp Int where
  lit = id
  neg = negate
  add = (+)

-- | Pretty printer.
instance Exp String where
  lit     = show
  neg s   = "-" ++ s
  add l r = "(" ++ l ++ "+" ++ r ++ ")"

-- | Evaluate an expression to an integer.
eval :: Int -> Int
eval = id

-- | Pretty print an expression.
pretty :: String -> String
pretty = id


--
-- * Integrate the object language and the variation metalanguage
--

-- | Variational interpretation of expressions.
instance Exp a => Exp (V a) where
  lit = plain . lit
  neg = vmap neg
  add = compose add

-- | The variation semantics of the evaluated expressions.
sem :: V Int -> V Int
sem = id

-- | The variation semantics of the pretty-printed expressions.
sem' :: V String -> V String
sem' = id


--
-- * Examples
--

chcA = chc "A"
chcB = chc "B"

-- | Plain expression.
e0 = neg (lit 1) `add` lit 2
-- | Introducing a choice.
e1 = e0 `add` chcA (lit 3) (lit 4)
-- | Synchronized choices.
e2 = neg e1 `add` chcA (lit 5) (lit 6)
-- | Independent choices.
e3 = neg e1 `add` chcB (lit 5) (lit 6)
-- | Nested plain expressions.
e4 = neg e1 `add` chcB (lit 5) (lit 6 `add` lit 7)
-- | Nested choice.
e5 = chcA (neg (lit 1)) (chcB (lit 2) (lit 4)) `add` lit 3
-- | Synchronized nested choice.
e6 = chcA (neg (lit 1)) (lit 2) `add` chcB (lit 3) (chcA (lit 4) (lit 5))
