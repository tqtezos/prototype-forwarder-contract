{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE CPP #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.DS.V1 where

#ifdef HAS_DSTOKEN

import Lorentz.Run (analyzeLorentz)
import Michelson.Text
import Lorentz
import Michelson.Analyzer (AnalyzerRes)

import qualified Lorentz.Contracts.DS.V1 as DS
import qualified Lorentz.Contracts.DS.V1.Registry.Types as Registry
import qualified Lorentz.Contracts.DS.V1.Token.Types as Token

import GHC.TypeLits (KnownSymbol)
import Prelude (Enum(..), ($))

import Data.Vinyl.Derived (Label)

-- | Construct a 'UParam' safely. See `mkUParam`.
toUParam
  :: forall name (a :: *) (entries :: [EntryPointKind]) s.
     ( KnownSymbol name, IsoValue a, KnownValue a, NoOperation a, NoBigMap a
     , LookupEntryPoint name entries ~ a
     , RequireUniqueEntryPoints entries
     )
  => Label name -> (a & s) :-> (UParam entries & s)
toUParam label = do
  pack
  push $ labelToMText label
  pair
  forcedCoerce_

-- | The number of sub-tokens to forward
type Parameter = Natural

-- | We need the addresses of:
-- - The sub-token contract, assumed to accept `DS.Parameter`
-- - The central wallet to transfer sub-tokens to
data Storage = Storage
  { subTokenContract :: Address -- ContractAddr DS.Parameter
  , centralWallet :: Address
  }
  deriving stock Eq
  deriving stock Generic
  deriving anyclass IsoValue

-- | `forcedCoerce_`
toWalletId :: (Address & s) :-> (Registry.WalletId & s)
toWalletId = forcedCoerce_

-- | Pushes self address to stack
selfAddress :: s :-> (Address & s)
selfAddress = do
  selfCalling @Parameter CallDefault
  address

-- | Convert to `Address` and amount to `Token.TransferParams`
toTokenTransfer :: (Address & Natural & s) :-> (Token.TransferParams & s)
toTokenTransfer = do
  constructT @Token.TransferParams
    ( fieldCtor $ do selfAddress; toWalletId; toNamed #from
    , fieldCtor $ do dup; toWalletId; toNamed #to
    , fieldCtor $ do duupX @2; toNamed #val
    )
  dip (dropN @2)

-- | Convert `Address` and amount to `UParam`
toTransferParameter :: (Address & Natural & s) :-> (UParam DS.Interface & s)
toTransferParameter = do
  toTokenTransfer
  toUParam #callTokenTransfer

-- | Run a transfer, given the amount and the `Storage`,
-- containing the central wallet address and sub token
-- contract address.
runTransfer :: (Natural & Storage & s) :-> (Operation & Storage & s)
runTransfer = do
  dip $ do
    getField #centralWallet
  swap
  toTransferParameter
  dip $ do
    getField #subTokenContract
    contractCalling @(DS.Parameter) $ Call @"Run"
    ifNone
      (failUnexpected (mkMTextUnsafe "not a DSToken"))
      (push (toEnum 0 :: Mutez))
  transferTokens

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
forwarderContract :: Contract Parameter Storage
forwarderContract = do
  unpair
  runTransfer
  dip nil
  cons
  pair

analyzeForwarder :: AnalyzerRes
analyzeForwarder = analyzeLorentz forwarderContract

#endif
