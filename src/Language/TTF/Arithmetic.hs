{-# LANGUAGE
      FlexibleInstances,
      NoMonomorphismRestriction,
      TypeSynonymInstances #-}

-- | Variational arithmetic expressions.
module Language.TTF.Arithmetic where

import Prelude hiding (div)
import qualified Prelude

import Language.TTF.CC.Choice

--
-- * Object language
--

-- | Simple arithmetic expressions in the tagless-final style.
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
pretty :: String -> IO ()
pretty = putStrLn


--
-- * Integrate the object language and the variation metalanguage
--

-- | Variational interpretation of expressions.
instance Exp a => Exp (V a) where
  lit = plain . lit
  neg = vmap neg
  add = compose add

-- | The variation semantics of the evaluated expressions.
veval :: V Int -> IO ()
veval = putStrLn . psem

-- | The variation semantics of the pretty-printed expressions.
vpretty :: V String -> IO ()
vpretty = putStrLn . psemS


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


--
-- * Simple properties
--

-- | Does the expression produce an even value?

newtype Even = Even Bool
  deriving (Eq,Show)

instance Exp Even where
  lit = Even . even
  neg = id
  add l r = Even (l == r)


-- | Is the expression negation free?

newtype NegFree = NegFree Bool
  deriving (Eq,Show)

instance Exp NegFree where
  lit _ = NegFree True
  neg _ = NegFree False
  add (NegFree l) (NegFree r) = NegFree (l && r)


--
-- * Extend object language with a division operation
--

class Div a where
  div :: a -> a -> a

instance Div Int where
  div = Prelude.div

instance Div String where
  div l r = "(" ++ l ++ "/" ++ r ++ ")"

instance Div a => Div (V a) where
  div = compose div

-- | An example expression.
d1 = e1 `div` (neg (lit 1) `add` chcB (lit 1) (lit 3))


--
-- * Safe semantics / divide by zero property
--

data DivZero = Ok Int | Error
  deriving (Eq,Show)
  
instance Exp DivZero where
  lit n             = Ok n
  neg (Ok n)        = Ok (negate n)
  neg _             = Error
  add (Ok l) (Ok r) = Ok (l + r)
  add _      _      = Error

instance Div DivZero where
  div _      (Ok 0) = Error
  div (Ok n) (Ok d) = Ok (div n d)
  div _      _      = Error
