{-# LANGUAGE
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      TypeSynonymInstances
  #-}

-- | A denotation used for stylized pretty printing.
module Language.TTF.Pretty where

import Data.Monoid (Monoid,(<>))
import GHC.Exts (IsString(..))

import System.Console.ANSI


-- * Denotations

-- | A stylized string is a list of substrings with corresponding styles.
type Stylized = [(Style,String)]

-- | Semantics domain of pretty printed programs, represented by a function
--   from some context to a stylized string.
newtype Pretty c a = Pretty { inCtx :: c -> Stylized }
  deriving Monoid

-- | Stylize a plain string.
style :: Style -> String -> Stylized
style s x = [(s,x)]

-- | String literals are interpreted as unstyled strings.
instance IsString Stylized where
  fromString = style []
instance IsString (Pretty c a) where
  fromString = Pretty . const . style []

-- | Pretty print with style to stdout.
pretty :: c -> Pretty c a -> IO ()
pretty c p = sequence_ [setSGR s >> putStr x | (s,x) <- inCtx p c]

-- | Get the pretty printed string without the style.
asString :: c -> Pretty c a -> String
asString c p = concatMap snd (inCtx p c)


-- * Useful Functions

-- | Add unstylized parentheses to a pretty printed value.
parens :: Stylized -> Stylized
parens = parens' []

-- | Add stylized parentheses to a pretty printed value.
parens' :: Style -> Stylized -> Stylized
parens' s p = style s "(" <> p <> style s ")"


-- * Styles

-- | Styles are represented by a list of ANSI codes.
type Style = [SGR]

-- | Empty style.
reset :: Style
reset = []

-- | Set intensity to bold.
bold :: Style
bold = [SetConsoleIntensity BoldIntensity]

-- | Set the foreground color.
color :: Color -> Style
color c = [SetColor Foreground Vivid c]
