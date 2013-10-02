
-- | Language components for simple literal values.
module Language.TTF.Literal where

import GHC.Exts (fromString)

import Language.TTF.Pretty


-- * Syntax

class BoolLiteral   r where bool   :: Bool   -> r Bool
class CharLiteral   r where char   :: Char   -> r Char
class IntLiteral    r where int    :: Int    -> r Int
class FloatLiteral  r where float  :: Float  -> r Int
class StringLiteral r where string :: String -> r String


-- * Pretty Printing Instances

instance BoolLiteral   (Pretty c) where bool   = fromString . show
instance CharLiteral   (Pretty c) where char   = fromString . show
instance IntLiteral    (Pretty c) where int    = fromString . show
instance FloatLiteral  (Pretty c) where float  = fromString . show
instance StringLiteral (Pretty c) where string = fromString . show
