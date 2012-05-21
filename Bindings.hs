{-# LANGUAGE
      NoMonomorphismRestriction,
      TypeSynonymInstances #-}

-- | Variational binding def-use checking.
module Bindings where

import Prelude hiding (seq)
import Data.List (intersperse,sort)
import Data.Set (Set,(\\),empty,intersection,singleton,toList,union)

import Variational

--
-- * Object language
--

-- | Variables.
type Var = String

-- | Simple "imperative" binding language.
class Bind a where
  def :: Var -> a     -- ^ Define a variable.
  use :: Var -> a     -- ^ Use a variable.
  sub :: a -> a       -- ^ Create a new sub-scope.
  seq :: a -> a -> a  -- ^ Sequence two statements.

-- | Pretty printer.
instance Bind String where
  def v = "@" ++ v
  use v = v
  sub a = "{" ++ a ++ "}"
  seq a b = a ++ ";" ++ b

-- | Pretty print a program.
pretty :: String -> IO ()
pretty = putStrLn


--
-- * Integrate object language with variation metalanguage.
--

instance Bind a => Bind (V a) where
  def = plain . def
  use = plain . use
  sub = vmap sub
  seq = compose seq

-- | Pretty print a variational program.
vpretty :: V String -> IO ()
vpretty = putStrLn . psemS


--
-- * Def-use binding property.
--
    
-- | Value computed by property.
data DefUse = DefUse
                (Set Var) -- ^ Defined in scope but not used.
                (Set Var) -- ^ Defined in scope and used.
                (Set Var) -- ^ Undefined but used.
                (Set Var) -- ^ Defined and unused in a different scope.

-- | Projection of DefUse to make the results easier to understand.
data Result = Result
                (Set Var) -- ^ Undefined but used.
                (Set Var) -- ^ Defined but not used.

-- | Compute result.
result :: DefUse -> Result
result (DefUse d _ ud uu) = Result ud (d `union` uu)

-- | Pretty print result types.
instance Show DefUse where
  show (DefUse d g ud uu) = "(" ++ (commas . map set) [d,g,ud,uu] ++ ")"
instance Show Result where
  show (Result u d) = "undefined: " ++ set u ++ "\tunused: " ++ set d

-- | Pretty printing helper functions.
commas = concat . intersperse ","
set s = "{" ++ (commas . sort . toList) s ++ "}"

-- | Check property.
instance Bind DefUse where
  def v = DefUse (singleton v) empty empty empty
  use v = DefUse empty empty (singleton v) empty
  
  sub (DefUse d _ ud uu) = DefUse empty empty ud (d `union` uu)
  
  seq (DefUse da ga uda uua) (DefUse db gb udb uub) =
    DefUse ((da \\ udb) `union` db) 
           (ga `union` gb `union` (da `intersection` udb))
           (uda `union` (udb \\ da))
           (uua `union` uub)

defUse :: V DefUse -> IO ()
defUse = putStrLn . psem . vmap result


--
-- * Examples
--

[a,b,c,d,e,f] = map (:[]) "abcdef"
chcA = chc "A"
chcB = chc "B"

p0 = def a `seq` sub (def b `seq` use a `seq` use b `seq` def c)
p1 = def a `seq` 
     chcA (def b) (def c) `seq`
     sub (chcB (use a) (use b) `seq`
          use c)
