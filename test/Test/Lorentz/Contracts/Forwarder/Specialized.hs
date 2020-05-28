{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Test.Lorentz.Contracts.Forwarder.Specialized
  ( spec_SpecializedForwarder
  ) where

import Lorentz

import Control.Monad (forM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Fmt (Buildable(..), listF)

import qualified Indigo.Contracts.AbstractLedger as AL
import qualified Lorentz.Contracts.Spec.AbstractLedgerInterface as AL

import qualified Lorentz.Contracts.Forwarder.Specialized as Fwd
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny as FwdAny
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny.Tez as FwdAnyTez
import qualified Lorentz.Contracts.ManagedLedger as ML
import Lorentz.Test
import Test.Hspec (Spec, describe, it)
import Util.Named ((.!))

import Test.Lorentz.Contracts.Forwarder.Common

type ForwarderRef = TAddress Natural
type ForwarderAnyRef = TAddress FwdAny.Parameter

originateSpecializedForwarder :: Address -> IntegrationalScenarioM (ForwarderRef)
originateSpecializedForwarder tokenAddress =
  lOriginate fwd "FA1.2 Specialized Forwarder" () (toMutez 0)
  where
    fwd = Fwd.specializedForwarderContract centralWallet tokenAddress

originateSpecializedAnyForwarder :: IntegrationalScenarioM (ForwarderAnyRef)
originateSpecializedAnyForwarder =
  lOriginate fwd "FA1.2 Specialized Any Token Forwarder" () (toMutez 0)
  where
    fwd = FwdAny.specializedAnyForwarderContract centralWallet

originateSpecializedAnyTezForwarder :: Mutez -> IntegrationalScenarioM (ForwarderAnyRef)
originateSpecializedAnyTezForwarder mutez' =
  lOriginate fwd "FA1.2 Specialized Any Token and Tez Forwarder" () mutez'
  where
    fwd = FwdAnyTez.specializedAnyTezForwarderContract centralWallet

originateAbstractLedger :: IntegrationalScenarioM (TAddress AL.Parameter)
originateAbstractLedger =
  lOriginate AL.abstractLedgerContract "Abstract ledger" storage (toMutez 0)
  where
    storage = AL.Storage
       { AL.ledger = BigMap $ Map.fromList [(masterAddress, 100500)]
       , AL.totalSupply = 100500
       }

originateManagedLedger :: IntegrationalScenarioM (TAddress ML.Parameter)
originateManagedLedger =
  lOriginate ML.managedLedgerContract "Managed ledger"
    (ML.mkStorage masterAddress mempty) (toMutez 0)

spec_SpecializedForwarder :: Spec
spec_SpecializedForwarder = do
  it "Successfully forwards Abstract ledger tokens to centralWallet" $
    integrationalTestExpectation $ do
      let amount = 100500
      tokenAddr <- originateAbstractLedger
      fwd <- originateSpecializedForwarder $ toAddress tokenAddr
      withSender masterAddress . lCallDef tokenAddr $
          AL.Transfer
          ( #from .! masterAddress
          , #to .! toAddress fwd
          , #value .! amount
          )
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      lCallDef fwd amount
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]

  it "Successfully forwards Managed ledger tokens to centralWallet" $
    integrationalTestExpectation $ do
      let amount = 100500
      tokenAddr <- originateManagedLedger
      fwd <- originateSpecializedForwarder $ toAddress tokenAddr
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      withSender masterAddress . lCallDef tokenAddr $
          ML.Mint
          ( #to .! toAddress fwd
          , #value .! amount
          )
      lCallDef fwd amount
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]

  it "Successfully forwards Abstract ledger tokens to centralWallet (FwdAny)" $
    integrationalTestExpectation $ do
      let amount = 100500
      tokenAddr <- originateAbstractLedger
      fwd <- originateSpecializedAnyForwarder
      withSender masterAddress . lCallDef tokenAddr $
          AL.Transfer
          ( #from .! masterAddress
          , #to .! toAddress fwd
          , #value .! amount
          )
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      lCallDef fwd $ FwdAny.mkParameter amount $ toAddress tokenAddr
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]

  it "Successfully forwards Managed ledger tokens to centralWallet (FwdAny)" $
    integrationalTestExpectation $ do
      let amount = 100500
      tokenAddr <- originateManagedLedger
      fwd <- originateSpecializedAnyForwarder
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      withSender masterAddress . lCallDef tokenAddr $
          ML.Mint
          ( #to .! toAddress fwd
          , #value .! amount
          )
      lCallDef fwd . FwdAny.mkParameter amount $ toAddress tokenAddr
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]

  it "Successfully forwards Abstract ledger tokens and Tez to centralWallet (FwdAnyTez)" $
    integrationalTestExpectation $ do
      let amount = 100500
          heldTezAmount = toEnum @Mutez 1000
          sentTezAmount = toEnum @Mutez 128
          noTez = toEnum @Mutez 0
          allTez = toEnum @Mutez . sum $ fromEnum <$>
            [ heldTezAmount
            , sentTezAmount
            ]
      tokenAddr <- originateAbstractLedger
      fwd <- originateSpecializedAnyTezForwarder heldTezAmount
      withSender masterAddress . lCallDef tokenAddr $
          AL.Transfer
          ( #from .! masterAddress
          , #to .! toAddress fwd
          , #value .! amount
          )
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      lTransfer (#from .! genesisAddress) (#to .! fwd) sentTezAmount CallDefault $
        FwdAny.mkParameter amount $
        toAddress tokenAddr
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]
      validate . Right $
          lExpectBalance tokenAddr noTez
      validate . Right $
          lExpectBalance consumer noTez
      validate . Right $
          lExpectBalance fwd noTez

  it "Successfully forwards Managed ledger tokens and Tez to centralWallet (FwdAnyTez)" $
    integrationalTestExpectation $ do
      let amount = 100500
          heldTezAmount = toEnum @Mutez 1000
          sentTezAmount = toEnum @Mutez 128
          noTez = toEnum @Mutez 0
          allTez = toEnum @Mutez . sum $ fromEnum <$>
            [ heldTezAmount
            , sentTezAmount
            ]
      tokenAddr <- originateManagedLedger
      fwd <- originateSpecializedAnyTezForwarder heldTezAmount
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      withSender masterAddress . lCallDef tokenAddr $
          ML.Mint
          ( #to .! toAddress fwd
          , #value .! amount
          )
      lTransfer (#from .! genesisAddress) (#to .! fwd) sentTezAmount CallDefault $
        FwdAny.mkParameter amount $
        toAddress tokenAddr
      lCallEP tokenAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]
      validate . Right $
          lExpectBalance tokenAddr noTez
      validate . Right $
          lExpectBalance consumer noTez
      validate . Right $
          lExpectBalance fwd noTez

