{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wno-orphans #-}

module GHC.Natural.Orphans where

import GHC.Generics

import Lorentz
import Michelson.Typed.Instr
import Michelson.Typed.Value

import Lorentz.Contracts.Forwarder.DS.V1 ()

import Data.Singletons (SingI)
import Prelude (id)

-- | Note: `from`, `to` are undefined.
--
-- This instance is only to placate the constraints for `ParameterHasEntryPoints`
instance Generic Natural where
  type Rep Natural = Rep ()
  from _ = error "Generic Natural: from not defined"
  to _ = error "Generic Natural: to not defined"

-- | Stub instance, defining @`Rep` (`Value` t)@ to be @`Rep` ()@
--
-- This instance is only to placate the constraints for `ParameterHasEntryPoints`
instance Generic (Value t) where
  type Rep (Value t) = Rep ()
  from = error "Generic (Value t): from not defined"
  to = error "Generic (Value t): to not defined"

instance IsoValue (Value' Instr a) where
  type ToT (Value' Instr a) = a
  toVal = id
  fromVal = id

instance (SingI t) => ParameterHasEntryPoints (Value t) where
  type ParameterEntryPointsDerivation (Value t) = EpdNone

instance ParameterHasEntryPoints Natural where
  type ParameterEntryPointsDerivation Natural = EpdNone
