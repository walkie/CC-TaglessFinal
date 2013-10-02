{-# LANGUAGE
      GeneralizedNewtypeDeriving
  #-}

-- | A denotation used for stylized pretty printing.
module Language.TTF.Pretty where

import Data.Monoid (Monoid)
import GHC.Exts (IsString(..))

import System.Console.ANSI


-- * Denotations

-- | Semantics domain of pretty printed programs, represented by a function
--   from some context to a list of strings with corresponding styles.
newtype Pretty c a = Pretty { inCtx :: c -> [(Style,String)] }
  deriving Monoid

-- | Stylize a plain string.
style :: Style -> String -> Pretty c a
style s x = Pretty $ \_ -> [(s,x)]

-- | String literals are interpreted as unstyled strings.
instance IsString (Pretty c a) where
  fromString = style []

-- | Pretty print with style to stdout.
pretty :: c -> Pretty c a -> IO ()
pretty c p = sequence_ [setSGR s >> putStr x | (s,x) <- inCtx p c]

-- | Get the pretty printed string without the style.
asString :: c -> Pretty c a -> String
asString c p = concatMap snd (inCtx p c)


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
