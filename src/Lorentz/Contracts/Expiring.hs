{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Expiring where

import Prelude hiding ((>>), drop)
import GHC.Generics (Generic) -- , Generic1)
import Text.Show (Show(..))
import Data.Functor.Classes
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec

import Lorentz
import Michelson.Text
import Michelson.Typed.EntryPoints

-- | Uses `parseEpAddress` with the remainder of the input
instance Read EpAddress where
  readPrec = do
    eEpAddress <- parseEpAddress . fromString <$> look
    case eEpAddress of
      Left err -> fail $ show err
      Right epAddress' -> return epAddress'

-- | Uses `readBinaryWith`
instance (Read a, NiceParameter r) => Read (View a r) where
  readPrec = readBinaryWith readPrec readPrec "View" $
    (\x -> View x . toContractRef @r @EpAddress)

-- | Either a `WrappedParameter`, which will fail if the contract has expired,
-- or `GetExpiration` (will never fail).
data Parameter cp
  = WrappedParameter !cp
  | GetExpiration (View () Timestamp)
  deriving  (Generic)

-- (NiceParameter cp, HasNoOp (ToT cp), HasNoNestedBigMaps (ToT cp))
instance NiceParameter cp => ParameterEntryPoints (Parameter cp) where
  parameterEntryPoints = pepNone

deriving instance Read cp => Read (Parameter cp)

deriving instance Show cp => Show (Parameter cp)

deriving instance IsoValue cp => IsoValue (Parameter cp)

data Storage st = Storage
  { wrappedStorage :: !st
  , expirationTime :: !Timestamp
  }
  deriving  (Generic)

-- | `coerce_` from `Storage`
unStorage :: Storage st & s :-> (st, Timestamp) & s
unStorage = coerce_

-- | `coerce_` to `Storage`
toStorage :: (st, Timestamp) & s :-> Storage st & s
toStorage = coerce_

deriving instance Show st => Show (Storage st)

deriving instance IsoValue st => IsoValue (Storage st)

-- | If the `Timestamp` is not before `now`, throws @"expired"@.
--
-- Caveat: `now` does not return the current time, it returns
-- the timestamp of the last baked block. This means that
-- `now` can be up to @time_between_blocks@ behind realtime.
--
-- Currently, @time_between_blocks@ is @60 seconds@ on mainnet
-- or @30@ seconds on testnet.
assertNotExpired :: Timestamp & s :-> Timestamp & s
assertNotExpired = do
  dup
  now
  assertLt $ mkMTextUnsafe "expired"

-- | Adds non-changeable expiration to a contract:
--
-- `Storage` contains an expiration `Timestamp` that can be
-- queried using `GetExpiration`.
--
-- Once the `Timestamp` is past, the contract is locked
--
-- Caveat: Up to error due to `now`, see `assertNotExpired` for more info
expiringContract :: forall cp st. IsoValue cp
  => Contract cp st
  -> Contract (Parameter cp) (Storage st)
expiringContract wrappedContract = do
  unpair
  caseT @(Parameter cp)
    ( #cWrappedParameter /-> do
        dip $ do
          unStorage
          unpair
          dip assertNotExpired
        pair
        swap
        dip $ do
          wrappedContract
          unpair
        swap
        dip $ do
          swap
          pair
          toStorage
        pair
    , #cGetExpiration /-> view_ $ do
        cdr
        unStorage
        cdr
    )

