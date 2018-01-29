module Data.Enum.Index
  ( Index
  , index
  , toNonEmptyArray
  , defaultCardinality
  , defaultToEnum
  , defaultFromEnum
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty, length, (!!))
import Data.Enum (class Enum, Cardinality(..), upFromIncluding)
import Data.Maybe (Maybe(Just))

newtype Index a = Index (NonEmptyArray a)

-- smart constructor for Index
index :: forall a. Bounded a => Enum a => Index a
index = Index $ fromNonEmpty $ upFromIncluding bottom

toNonEmptyArray :: forall a. Index a -> NonEmptyArray a
toNonEmptyArray (Index xs) = xs

-- | Runs in `O(1)`
defaultCardinality :: forall a. Index a -> Cardinality a
defaultCardinality = Cardinality <<< length <<< toNonEmptyArray

-- | Runs in `O(1)`
defaultToEnum :: forall a. Index a -> Int -> Maybe a
defaultToEnum index' i = toNonEmptyArray index' !! i

-- | Runs in `O(logn)`
defaultFromEnum :: forall a. Ord a => Index a -> a -> Int
defaultFromEnum (Index xs) val = binSearch 0 (length xs - 1)
  where
  binSearch low high =
    if (xs !! mid > Just val) then binSearch low $ mid - 1
    else if (xs !! mid < Just val) then binSearch (mid + 1) high
    else mid
    where
    mid = low + ((high - low) `div` 2)
