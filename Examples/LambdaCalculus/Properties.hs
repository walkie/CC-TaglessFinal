
-- | Compositional property checking of lambda calculus.
module Examples.LambdaCalculus.Properties where

import qualified Data.Set as S

import Data.Variational
import Examples.LambdaCalculus.Language

-- 
-- * Free variables
--

-- | Return the free variables in an expression.

newtype FV = FV (S.Set Var)
  deriving (Eq,Show)

instance LC FV where
  ref                 = FV . S.singleton
  abs v (FV vs)       = FV (S.delete v vs)
  app (FV ls) (FV rs) = FV (ls `S.union` rs)

fv :: V FV -> IO ()
fv = putStrLn . psem
