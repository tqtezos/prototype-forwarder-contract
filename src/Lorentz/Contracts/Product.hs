{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS -Wno-simplifiable-class-constraints #-}

module Lorentz.Contracts.Product where

import Prelude hiding ((>>), drop)
import GHC.Generics (Generic)
import Text.Show (Show(..))
import Data.Functor.Classes
import Text.Read (Read(..))

import Lorentz
import Michelson.Text
import Michelson.Typed.EntryPoints
import Michelson.Typed.Scope
import Tezos.Address

-- | Parameter for either given `Contract`
data (:|:) cp1 cp2
  = LeftParameter !cp1
  | RightParameter !cp2
  deriving  (Generic)

instance ( NiceParameter cp1
         , HasNoOp (ToT cp1)
         , HasNoNestedBigMaps (ToT cp1)
         , NiceParameter cp2
         , HasNoOp (ToT cp2)
         , HasNoNestedBigMaps (ToT cp2)
         ) => ParameterHasEntryPoints (cp1 :|: cp2) where
  -- parameterEntryPoints = pepNone
  type ParameterEntryPointsDerivation (cp1 :|: cp2) = EpdNone

deriving instance (Read cp1, Read cp2) => Read (cp1 :|: cp2)

deriving instance (Show cp1, Show cp2) => Show (cp1 :|: cp2)

deriving instance (IsoValue cp1, IsoValue cp2) => IsoValue (cp1 :|: cp2)

-- | Parameter for both given `Contract`s
data (:&:) st1 st2 = (:&:)
  { leftStorage  :: !st1
  , rightStorage :: !st2
  }
  deriving  (Generic)

deriving instance (Read cp1, Read cp2) => Read (cp1 :&: cp2)

deriving instance (Show cp1, Show cp2) => Show (cp1 :&: cp2)

deriving instance (IsoValue cp1, IsoValue cp2) => IsoValue (cp1 :&: cp2)

-- | `forcedCoerce_` from `(:&:)`
unStorage :: (st1 :&: st2) & s :-> (st1, st2) & s
unStorage = forcedCoerce_

-- | `forcedCoerce_` to `(:&:)`
toStorage :: (st1, st2) & s :-> (st1 :&: st2) & s
toStorage = forcedCoerce_

-- | The (independent) product of two contracts:
-- accepting parameters from either (`:|:`) and holding storage
-- for both (`:&:`)
productContract :: forall cp1 st1 cp2 st2. (IsoValue cp1, IsoValue cp2)
  => Contract cp1 st1
  -> Contract cp2 st2
  -> Contract (cp1 :|: cp2) (st1 :&: st2)
productContract wrappedLeft wrappedRight = do
  unpair
  caseT @(cp1 :|: cp2)
    ( #cLeftParameter /-> do
        dip $ do
          unStorage
          unpair
        pair
        swap
        dip $ do
          wrappedLeft
          unpair
        swap
        dip $ do
          swap
          pair
          toStorage
        pair
    , #cRightParameter /-> do
        dip $ do
          unStorage
          unpair
          swap
        pair
        swap
        dip $ do
          wrappedRight
          unpair
        swap
        dip $ do
          pair
          toStorage
        pair
    )

