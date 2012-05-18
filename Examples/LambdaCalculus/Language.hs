{-# LANGUAGE
      FlexibleInstances,
      NoMonomorphismRestriction,
      TypeSynonymInstances #-}

-- | Variational lambda calculus expressions.
module Examples.LambdaCalculus.Language where

import Prelude hiding (abs,id,fst,snd)
import qualified Prelude

import Data.Variational

--
-- * Syntax of object language
--

-- | Variables.
type Var = String

-- | Lambda calculus expressions in the tagless-final style.
class LC a where
  ref :: Var -> a
  abs :: Var -> a -> a
  app :: a -> a -> a

-- | Pretty printer.
instance LC String where
  ref v = v
  abs v b = "λ" ++ v ++ "." ++ b
  app l r = "(" ++ l ++ ") (" ++ r ++ ")"

-- | Pretty print an expression.
pretty :: String -> IO ()
pretty = putStrLn


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
