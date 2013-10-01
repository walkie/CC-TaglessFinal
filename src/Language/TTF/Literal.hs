
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

instance BoolLiteral   Pretty where bool   = fromString . show
instance CharLiteral   Pretty where char   = fromString . show
instance IntLiteral    Pretty where int    = fromString . show
instance FloatLiteral  Pretty where float  = fromString . show
instance StringLiteral Pretty where string = fromString . show
