
-- | Compositional property checking of variational arithmetic expressions.
module Examples.Arithmetic.Properties where

import Data.Variational
import Examples.Arithmetic.Language

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
