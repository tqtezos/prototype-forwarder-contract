{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.Specialized.Tez.Flush where

import Lorentz

import Text.Show

data Parameter
  = Default !()
  | F !()
  deriving (Eq, Show, Generic, IsoValue)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type Storage = ()

specializedTezFlushForwarderContract :: Address -> ContractCode Parameter Storage
specializedTezFlushForwarderContract centralWallet = do
  car
  forcedCoerce_ @Parameter @(Either () ())
  ifLeft
    nil
    (do
      nil
      push centralWallet
      contract @()
      ifNone
        failWith
        (do
          balance
          unit
          transferTokens @()
          cons
        )
    )
  pair

