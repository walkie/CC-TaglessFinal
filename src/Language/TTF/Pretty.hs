{-# LANGUAGE
      GeneralizedNewtypeDeriving
  #-}

-- | A denotation used for stylized pretty printing.
module Language.TTF.Pretty where

import Data.Monoid (Monoid)
import GHC.Exts (IsString(..))

import System.Console.ANSI


-- * Denotations

-- | Semantics domain of pretty printed programs, represented by a list of
--   strings with corresponding styles.
newtype Pretty a = Pretty [(Style,String)]
  deriving (Eq,Show,Monoid)

-- | String literals are interpreted as unstyled strings.
instance IsString (Pretty a) where
  fromString s = Pretty [([],s)]

-- | Pretty print with style to stdout.
pretty :: Pretty a -> IO ()
pretty (Pretty p) = sequence_ [setSGR c >> putStr s | (c,s) <- p]

-- | Get the pretty printed string without the style.
asString :: Pretty a -> String
asString (Pretty p) = concatMap snd p


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
