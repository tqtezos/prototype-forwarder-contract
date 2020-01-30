{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS -Wall -Wno-orphans #-}

module GHC.Natural.Orphans where

import GHC.Generics

import Lorentz hiding (SomeContract(..))
import Lorentz.Run (analyzeLorentz)
import Lorentz.Base (SomeContract(..))
import Michelson.Analyzer (AnalyzerRes)
-- import Michelson.Optimizer
-- import Michelson.Typed.Instr (toFullContract)
-- import qualified Michelson.Typed.Instr as Instr
-- import Michelson.Typed.Annotation
-- import Michelson.Typed.Value
-- import Michelson.Typed.Scope
import Michelson.Typed.Value
import Michelson.Typed.T
-- import Michelson.TypeCheck.Types (SomeContract(..))
import Michelson.Text

import qualified Lorentz.Contracts.DS.V1 as DS
import Lorentz.Contracts.Forwarder.DS.V1 (toTransferParameter)

import Data.Type.Equality
import Data.Typeable
import Prelude (Enum(..), Eq(..), ($), String, show)
import Data.Singletons (SingI)

-- | Note: `from`, `to` are undefined
instance Generic Natural where
  type Rep Natural = Rep ()
  from _ = error "Generic Natural: from not defined"
  to _ = error "Generic Natural: to not defined"

-- -- | Note: `from`, `to` are undefined
-- instance Generic (Value ('Tc 'CNat)) where
--   type Rep (Value ('Tc 'CNat)) = Rep Natural
--   from (VC (CvNat xs)) = from xs
--   to = VC . CvNat . to

-- instance (SingI ct, Typeable ct) => ParameterHasEntryPoints (Value ('Tc ct)) where
--   type ParameterEntryPointsDerivation (Value ('Tc ct)) = EpdNone
  -- parameterEntryPoints = pepNone

-- | Stub instance, defining @`Rep` (`Value` t)@ to be @`Rep` ()@
instance Generic (Value t) where
  type Rep (Value t) = Rep ()
  from = error "Generic (Value t): from not defined"
  to = error "Generic (Value t): to not defined"

instance (SingI t) => ParameterHasEntryPoints (Value t) where
  type ParameterEntryPointsDerivation (Value t) = EpdNone

instance ParameterHasEntryPoints Natural where
  type ParameterEntryPointsDerivation Natural = EpdNone
  -- parameterEntryPoints = pepNone

