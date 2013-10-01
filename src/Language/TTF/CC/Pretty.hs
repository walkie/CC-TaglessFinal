
-- | Some styles for choice calculus expressions.
module Language.TTF.CC.Pretty where

import System.Console.ANSI (Color(..))

import Language.TTF.Pretty


styleOp  = color Blue
styleKey = color Blue ++ bold
styleDim = color Green
styleTag = color Green
styleVar = color Red
