{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses,
      OverloadedStrings,
      PatternGuards
  #-}

-- | Variational values and a simple variation metalanguage.
module Language.TTF.CC.Choice where

import Data.List (intercalate,intersperse)
import Data.Monoid ((<>),mconcat)
import GHC.Exts (fromString)

import Language.TTF.Literal
import Language.TTF.Pretty


-- * Variational Values

-- | A tag represents a single configuration option.
type Tag = String

-- | A configuration is identified by a list of tags (the selected
--   configuration options).
type Config = [Tag]


-- * Choices

-- | A choice introduces a variation point as a choice between alternatives.
--   Ultimately, when a configuration is selected, each choice will be
--   replaced by one of its alternatives, but there are many different ways
--   of relating the selected configuration to alternatives.


-- ** Binary choices

-- | A binary choice is associated with a single tag.
--   The first alternative is used if the tag is selected,
--   the second alternative is used if the tag is not selected.
class ChcT r where
  chcT :: Tag -> r a -> r a -> r a

instance ChcT (Pretty c) where
  chcT t a b = fromString t <> "‹" <> a <> "," <> b <> "›"


-- ** Dimensioned choices

-- | A dimension defines a list of mutually exclusive tags.
type Dim = String

-- | A dimensioned choice is associated with a dimension.
--   Each alternative corresponds to the tag at the same position
--   in its binding dimension declaration.
class ChcD r where
  dim  :: Dim -> [Tag] -> r a -> r a
  chcD :: Dim -> [r a] -> r a

instance ChcD (Pretty c) where
  dim  d ts e = fromString d <> "‹" <> fromString (intercalate "," ts) <> "› " <> e
  chcD d as   = fromString d <> "‹" <> mconcat (intersperse "," as) <> "›"


-- ** Formula choices

-- | Boolean tag expressions.
data Formula = FTag Tag
             | FNot Formula
             | FAnd Formula Formula
             | FOr  Formula Formula
  deriving Eq

instance Show Formula where
  show f = case f of
      FAnd f g -> paren f ++ "∧" ++ paren g
      FOr  f g -> paren f ++ "∨" ++ paren g
      f        -> paren f
    where
      paren (FTag t) = t
      paren (FNot f) = "¬" ++ paren f
      paren f        = "(" ++ show f ++ ")"

-- | A formula choice is associated with a boolean tag formula.
--   If the formula is satisfied by the selection, the first alternative
--   is selected; otherwise, the second alternative is selected.
class ChcF r where
  chcF :: Formula -> r a -> r a -> r a

instance ChcF (Pretty c) where
  chcF f a b = fromString (show f) <> "‹" <> a <> "," <> b <> "›"

{-

--
-- * Decisions
--

type Dim  = String
data Tag  = L | R     deriving (Eq,Ord,Show)
data QTag = Q Dim Tag deriving (Eq,Ord)
type Dec  = [QTag]

-- | Attempt to merge dimension-sorted decisions:
--     Just d  -- if decisions are compatible
--     Nothing -- otherwise
merge :: Dec -> Dec -> Maybe Dec
merge (t@(Q dt _):ts) (u@(Q du _):us)
  | t  == u   = fmap (t:) (merge ts us)
  | dt <  du  = fmap (t:) (merge ts (u:us))
  | dt >  du  = fmap (u:) (merge (t:ts) us)
  | otherwise = Nothing
merge ts [] = Just ts
merge [] us = Just us

instance Show QTag where
  show (Q d t) = d ++ "." ++ show t


--
-- * Variational values
--

-- | A variational 'a' value is a mapping from decisions to variants of type 'a'.
type V a = [(Dec,a)]

-- | Plain value.
plain :: a -> V a
plain a = [([],a)]

-- | Map a function across all variants.
vmap :: (a -> b) -> V a -> V b
vmap f va = [(d,f a) | (d,a) <- va]

-- | Add a variant if two decisions merge successfully.
ifMerge :: a -> Dec -> Dec -> V a
ifMerge a ts us | Just vs <- merge ts us = [(vs,a)]
                | otherwise              = []

-- | Compose two variational values.
compose :: (a -> b -> c) -> V a -> V b -> V c
compose f va vb = do 
    (da,a) <- va
    (db,b) <- vb
    ifMerge (f a b) da db

-- | Qualify every compatible decision with an additional tag.
qualify :: QTag -> V a -> V a
qualify q va = do 
    (d,a) <- va
    ifMerge a [q] d

-- | Get a pretty version of the semantics.
psem :: Show a => V a -> String
psem = psemS . vmap show

-- | Pretty semantics with string values.
psemS :: V String -> String
psemS = unlines . map entry
  where entry (d,s) = concat (intersperse "," (map show d)) ++ "  ==>  " ++ s

-}
