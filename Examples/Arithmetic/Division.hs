{-# LANGUAGE
      NoMonomorphismRestriction,
      TypeSynonymInstances #-}

-- | Extension of arithmetic expressions with division and checking for
--   divide-by-zero errors.
module Examples.Arithmetic.Division where

import Prelude hiding (div)
import qualified Prelude

import Data.Variational
import Examples.Arithmetic.Language

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
