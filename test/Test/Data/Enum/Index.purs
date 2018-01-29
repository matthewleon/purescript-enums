module Test.Data.Enum.Index (testIndex) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array.NonEmpty (toArray)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, toEnum)
import Data.Enum.Index (Index, cardinalityFromIndex, fromEnumFromIndex, index, toEnumFromIndex, toNonEmptyArray)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)

data T = A | B | C | D | E

derive instance eqT  :: Eq  T
derive instance ordT :: Ord T

instance enumT :: Enum T where
  succ A = Just B
  succ B = Just C
  succ C = Just D
  succ D = Just E
  succ E = Nothing

  pred A = Nothing
  pred B = Just A
  pred C = Just B
  pred D = Just C
  pred E = Just D

instance boundedT :: Bounded T where
  bottom = A
  top = E

instance boundedEnumT :: BoundedEnum T where
  cardinality = cardinalityFromIndex indexT
  toEnum = toEnumFromIndex indexT
  fromEnum = fromEnumFromIndex indexT

indexT :: Index T
indexT = index

testIndex :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
testIndex = do
  log "toNonEmptyArray"
  assert $ toArray (toNonEmptyArray indexT) == [A, B, C, D, E]

  log "cardinalityFromIndex"
  assert $ cardinality == (Cardinality 5 :: Cardinality T)

  log "toEnumFromIndex"
  assert $ toEnum 0 == Just A
  assert $ toEnum 1 == Just B
  assert $ toEnum 2 == Just C
  assert $ toEnum 3 == Just D
  assert $ toEnum 4 == Just E

  log "fromEnumFromIndex"
  assert $ fromEnum A == 0
  assert $ fromEnum B == 1
  assert $ fromEnum C == 2
  assert $ fromEnum D == 3
  assert $ fromEnum E == 4
