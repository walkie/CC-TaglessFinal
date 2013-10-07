{-# LANGUAGE
      FlexibleInstances,
      NoMonomorphismRestriction,
      OverloadedStrings,
      LiberalTypeSynonyms
  #-}

-- | Variational untyped lambda calculus expressions.
module Language.TTF.Lambda where

import Data.Monoid ((<>))
import qualified Data.Set as S
import GHC.Exts (fromString)

import Language.TTF.Pretty


-- * Syntax

-- | Variables
type Var = String

-- | Lambda calculus expressions in the tagless-final style.
class Lambda r where
  ref :: Var -> r a
  lam :: Var -> r a -> r a
  app :: r a -> r a -> r a

data LambdaCtx = Top | InL | InR
  deriving Eq

-- | Pretty printer.
instance Lambda (Pretty LambdaCtx) where
  ref v   = fromString v
  lam v b = Pretty $ \c ->
    (if c == InL then parens else id)
    ("λ" <> fromString v <> "." <> inCtx b Top)
  app l r = Pretty $ \c -> 
    (if c == InR then parens else id)
    (inCtx l InL <> " " <> inCtx r InR)



--
-- * Semantics of object language
--

-- | Explicit representation of lambda calculus expressions.
data Exp = Ref Var | Abs Var Exp | App Exp Exp

instance Show Exp where
  show (Ref v)   = ref v
  show (Abs v b) = abs v (show b)
  show (App l r) = app (show l) (show r)

-- | Translate into explicit representation.
instance LC Exp where
  ref = Ref
  abs = Abs
  app = App

-- | Expression reduction.
reduce :: [(Var,Exp)] -> Exp -> Exp
reduce m (Ref v)   =
    case lookup v m of
      Just e    -> e
      otherwise -> Ref v
reduce m (Abs v e) = Abs v (reduce ((v,Ref v):m) e)
reduce m (App l r) =
    case reduce m l of
      Abs v e -> (reduce ((v,r'):m) e)
      l'      -> App l' r'
  where r' = reduce m r

-- | Evaluate an expresion.
eval :: Exp -> Exp
eval = reduce []


--
-- * Integrate with variation metalanguage
--

instance LC a => LC (V a) where
  ref = plain . ref
  abs = vmap . abs
  app = compose app

-- | Variation semantics of evaluated expressions.
veval :: V Exp -> IO ()
veval = putStrLn . psem . vmap eval

-- | Variation semantics of pretty-printed expressions.
vpretty :: V String -> IO ()
vpretty = putStrLn . psemS


--
-- * Examples
--

-- | Variables.
[a,b,c,x,y,z] = map (:[]) "abcxyz"

-- | Basic functions.
id  = abs x (ref x)
fst = abs x (abs y (ref x))
snd = abs x (abs y (ref y))

-- | Smart constructors.
app2 f = app . app f
app3 f = app . app . app f
chcA = chc "A"
chcB = chc "B"

-- | Plain expression.
e0 = app2 fst id snd
-- | Introducing a choice.
e1 = app2 (chcA fst snd) (ref x) (ref y)


-- 
-- * Compositional property checking
--

-- ** Free variables

newtype FV = FV (S.Set Var)
  deriving (Eq,Show)

instance LC FV where
  ref                 = FV . S.singleton
  abs v (FV vs)       = FV (S.delete v vs)
  app (FV ls) (FV rs) = FV (ls `S.union` rs)

fv :: V FV -> IO ()
fv = putStrLn . psem
