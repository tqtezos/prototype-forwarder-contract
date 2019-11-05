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
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder where

import Lorentz
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Types as Athens

import Prelude (Show(..), Enum(..))

deriving instance Show Athens.Parameter
-- deriving instance (Show a, Show r) => Show (View a r)
-- deriving instance Show a => Show (ContractAddr a)

-- | We need the addresses of:
-- - The sub-token contract, assumed to accept `Athens.Parameter`
-- - The Tezos Wallet to process refunds (assuming this contract is authorized to call it)
-- - The central wallet to transfer sub-tokens to
data Storage = Storage
  { subTokenContract :: ContractAddr Athens.Parameter
  , tezosWallet :: ContractAddr RefundParameters
  , centralWallet :: Address
  }
  deriving stock Eq
  deriving stock Show
  deriving stock Generic
  deriving anyclass IsoValue

-- | What's required to refund mutez from the Tezos Wallet
type RefundParameters = ("amount" :! Mutez, "to" :! Address)

-- | The number of sub-tokens to transfer
type Parameter = Natural

-- | `coerce_` to `Athens.TransferParams`
toTransferParams :: (Address & Address & Natural & s) :-> (Athens.TransferParams & s)
toTransferParams = do
  dip pair
  pair
  coerce_ @(Address, (Address, Natural)) @Athens.TransferParams

-- | Run `Athens.TransferParams` with a @`ContractAddr` `Athens.Parameter`@,
-- from `Address`, to `Address`, and number of sub-tokens
runTransferParams :: (ContractAddr Athens.Parameter & Address & Address & Natural & s) :-> (Operation & s)
runTransferParams = do
  dip toTransferParams
  swap
  dip (push (toEnum 0 :: Mutez))
  wrap_ @Athens.Parameter #cTransfer
  transferTokens

-- | Run `Athens.TransferParams` on the given `Parameter` and `Storage`, where
-- from is `sender` and to is `centralWallet`
runStorageTransferParams :: (Parameter & Storage & s) :-> (Operation & Storage & s)
runStorageTransferParams = do
  swap
  getField #subTokenContract
  dip (getField #centralWallet >> dip swap >> sender)
  runTransferParams

-- | Derive `RefundParameters` and transfer arguments from
-- the number of `Mutez` to refund and `Storage`
toRefundParameters :: (Mutez & Storage & s) :-> (RefundParameters & Mutez & ContractAddr RefundParameters & Storage & s)
toRefundParameters = do
  dip sender
  pair
  coerce_ @(Mutez, Address) @RefundParameters
  dip (do
    push (toEnum 0 :: Mutez)
    dip (getField #tezosWallet)
      )

-- | Process a refund, given the refund amount in `Mutez` and `Storage`
processRefund :: (Mutez & Storage & s) :-> (Operation & Storage & s)
processRefund = do
  toRefundParameters
  transferTokens

-- | Given a method to calculate the number of `Mutez` to refund from the number
-- of sub-tokens transferred, produce a forwarder contract.
forwarderContract :: (forall s. (Natural & s) :-> (Mutez & s)) -> Contract Parameter Storage
forwarderContract calculateGasCost = do
  unpair
  dup
  dip runStorageTransferParams
  swap
  dip (do
    calculateGasCost
    processRefund
    dip nil
    cons
      )
  cons
  pair

