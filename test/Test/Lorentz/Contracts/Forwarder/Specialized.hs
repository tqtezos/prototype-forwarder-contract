{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Lorentz.Contracts.DS.V1.Registry (InvestorId)
import qualified Lorentz.Contracts.Forwarder.Specialized as Fwd
import qualified Lorentz.Contracts.ManagedLedger as ML
import Lorentz.Test
import Test.Hspec (Spec, describe, it)
import Util.Named ((.!))

import Test.Lorentz.Contracts.Forwarder.Common
import Test.Lorentz.Contracts.Forwarder.DS.Common

type ForwarderRef = TAddress Natural

originateSpecializedForwarder :: Address -> IntegrationalScenarioM (ForwarderRef)
originateSpecializedForwarder tokenAddress =
  lOriginate fwd "FA1.2 Specialized Forwarder" () (toMutez 0)
  where
    fwd = Fwd.specializedForwarderContract centralWallet tokenAddress

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

  it "Successfully forwards DS Tokens to centralWallet" $
    integrationalTestExpectation $ do
      let amount = 100500
      dsAddr <- originateDSToken
      fwd <- originateSpecializedForwarder $ toAddress dsAddr
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      registerInvestor dsAddr investor1 $
          [(toAddress fwd), centralWallet]
      mint dsAddr (toAddress fwd) amount
      lCallDef fwd amount
      lCallEP dsAddr (Call @"GetBalance") (mkView (#owner .! centralWallet) consumer)
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]
