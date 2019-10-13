{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.DS.V1.Specialized where

import Lorentz
import Lorentz.Base (analyzeLorentz)
-- import Lorentz.CompileExts
import Michelson.Analyzer (AnalyzerRes)
import Michelson.Optimizer
import Michelson.Typed.Instr (toFullContract)

import qualified Lorentz.Contracts.DS.V1 as DS
import Lorentz.Contracts.Forwarder.DS.V1 (toTransferParameter)

import Prelude (Enum(..), ($))


-- | The number of sub-tokens to forward
type Parameter = Natural

-- | We have the addresses of:
-- - The sub-token contract, assumed to accept `DS.Parameter`
-- - The central wallet to transfer sub-tokens to
type Storage = ()

-- changed
runSpecializedTransfer :: Address -> ContractAddr DS.Parameter -> (Natural & s) :-> (Operation & s)
runSpecializedTransfer centralWalletAddr' contractAddr' = do
  push centralWalletAddr'
  toTransferParameter
  dip $ do
    push contractAddr'
    push (toEnum 0 :: Mutez)
  transferTokens

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
specializedForwarderContract :: Address -> ContractAddr DS.Parameter -> Contract Parameter Storage
specializedForwarderContract centralWalletAddr' contractAddr' = do
  car
  runSpecializedTransfer centralWalletAddr' contractAddr'
  dip nil
  cons
  dip unit
  pair

specializedForwarderCompilationWay :: LorentzCompilationWay Parameter Storage
specializedForwarderCompilationWay =
  LorentzCompilationWay (toFullContract . optimize . compileLorentzContract)

-- forwarderDocumentation :: LText

analyzeSpecializedForwarder :: Address -> ContractAddr DS.Parameter -> AnalyzerRes
analyzeSpecializedForwarder centralWalletAddr' contractAddr' =
  analyzeLorentz $ specializedForwarderContract centralWalletAddr' contractAddr'

