{-# LANGUAGE
      FlexibleInstances,
      PatternGuards,
      TypeSynonymInstances #-}

-- | Variational values and a simple variation metalanguage.
module Language.TTF.CC.Choice where

import Data.List (intersperse)

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


--
-- * Variation annotations
--

-- | Tagless final representation of choices.
class CC a where
  -- | A choice introduces a point of variation in some dimension.
  --   All choices in the same dimension are synchronized.
  chc :: Dim -> a -> a -> a

-- | Variational evaluator
instance CC (V a) where
  chc d l r = qualify (Q d L) l ++ qualify (Q d R) r
