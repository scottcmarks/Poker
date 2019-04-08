{-|
Module      : Data.Map.PokerExtras
Description : Map composition and inverse
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Potentially lossy composition of maps-as-functions, and inverse map.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Map.PokerExtras (
 -- * Composition of maps as functions
    (⊙)
 -- * Composition of maps as arrows (inv ⊙)
  , (⊚)
 -- * Inverse of map as function
  , inv
 -- * Doubly-indexed map
  , Map2
  , insert2
  , lookup2
  , inv2
  )
where
import           Data.Map                 (empty, findWithDefault, fromList,
                                           insert, lookup, toList)
import           Data.Map.Strict.Internal (Map (..), foldlWithKey')
import           RIO                      (Maybe (..), Ord, Ordering (..),
                                           compare, flip, map, maybe, mempty,
                                           (&), (.))


-- | Composition of maps as functions (Unicode)
(⊙) :: Ord b => Map b c -> Map a b -> Map a c
f ⊙ g = go g
  where
    go Tip = Tip
    go (Bin s k v l r) = go' f
      where
        go' Tip = Tip
        go' (Bin _ k' x' l' r') = case compare v k' of
            EQ -> (Bin s k x' (go l) (go r))
            LT -> go' l'
            GT -> go' r'
infixr 7 ⊙

-- | Composition of maps as arrows (Unicode)
--   The arity and fixity is chosen so a to allow chains with <^.>/⋈
(⊚) :: Ord b => Map a b -> Map b c -> Map a c
(⊚) = flip (⊙)
infixl 8 ⊚


-- | Inverse of map as function
inv :: Ord a => Map k a -> Map a k
inv = fromList . map swap . toList
        where swap ~(x,y) = (y,x)


-- | Map2 -- nested 'Map's
type Map2 k1 k2 a = Map k1 (Map k2 a)

-- | Insert a value in a 'Map2' as indexed by two keys
insert2 :: (Ord k1, Ord k2) =>
    k1
 -- ^ first key
 -> k2
 -- ^ second key
 -> a
 -- ^ new value
 -> Map2 k1 k2 a
 -- ^ map to be updated
 -> Map2 k1 k2 a
insert2 key1 key2 value m =
    findWithDefault empty key1 m    -- find or create inner map
  & insert key2 value               -- insert value into inner map
  & flip (insert key1) m            -- insert inner map into outer map


-- | Retrieve a value in a 'Map2' as indexed by two keys
lookup2 :: (Ord k1, Ord k2) =>
    k1
 -- ^ first key
 -> k2
 -- ^ second key
 -> Map2 k1 k2 a
 -- ^ map to be accessed
 -> Maybe a
lookup2 key1 key2 m = m & lookup key1 & maybe Nothing (lookup key2)


-- | Invert a map-to-pairs to a Map2
inv2 :: (Ord a, Ord b) =>
    Map k (a,b)
 -- ^ Map to a pair
 -> Map2 a b k
inv2 mPair = foldlWithKey' ins mempty mPair
  where
    ins map2 k (a,b) = insert2 a b k map2
