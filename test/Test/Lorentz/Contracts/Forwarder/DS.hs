{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Lorentz.Contracts.Forwarder.DS
  ( spec_SpecializedDSForwarder
  , spec_ValidatedDSForwarder
  ) where

import Lorentz

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Set (Set)
import Fmt (Buildable(..), listF)

import Lorentz.Contracts.DS.V1.Registry (InvestorId)
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as Fwd
import qualified Lorentz.Contracts.Forwarder.DS.V1.Validated as Validated
import Lorentz.Contracts.Validate.Reception (Parameter(..), Whitelist)
import Lorentz.Contracts.Product ((:|:)(..))
import Lorentz.Test
import Test.Hspec (Spec, describe, it)
import Util.Named ((.!))

import Test.Lorentz.Contracts.Forwarder.Common
import Test.Lorentz.Contracts.Forwarder.DS.Common

type ForwarderRef = TAddress Natural
type ValidatedForwarderRef = TAddress Validated.Parameter

originateSpecializedForwarder :: DSRef -> IntegrationalScenarioM (ForwarderRef)
originateSpecializedForwarder dsRef =
  lOriginate fwd "DS Specialized Forwarder" () (toMutez 0)
  where
    fwd = Fwd.specializedForwarderContract centralWallet (callingDefTAddress dsRef)

originateValidatedForwarder
  :: DSRef -> [InvestorId] -> IntegrationalScenarioM (ValidatedForwarderRef)
originateValidatedForwarder dsRef investorList =
  lOriginate fwd "DS Validated Forwarder" storage (toMutez 0)
  where
    storage = Validated.mkStorage investorList
    fwd = Validated.validatedForwarderContract centralWallet (callingDefTAddress dsRef)

spec_SpecializedDSForwarder :: Spec
spec_SpecializedDSForwarder = do
  it "Successfully forwards funds to centralWallet" $
    integrationalTestExpectation $ do
      dsAddr <- originateDSToken
      fwd <- originateSpecializedForwarder dsAddr
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      registerInvestor dsAddr investor1 $
          [(toAddress fwd), centralWallet]
      mint dsAddr (toAddress fwd) 100500
      lCallDef fwd (100500 :: Natural)
      lCallEP dsAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [100500]

spec_ValidatedDSForwarder :: Spec
spec_ValidatedDSForwarder = do
  it "Successfully forwards funds to centralWallet" $
    integrationalTestExpectation $ do
      let amount = 100500
      dsAddr <- originateDSToken
      fwd <- originateValidatedForwarder dsAddr [investor1]
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      registerInvestor dsAddr investor1 $
          [(toAddress fwd), centralWallet]
      mint dsAddr (toAddress fwd) amount
      lCallDef fwd $ LeftParameter amount
      lCallEP dsAddr (Call @"GetBalance") $
          mkView (#owner .! centralWallet) consumer
      validate . Right $
          lExpectViewConsumerStorage consumer [amount]

  it "Successfully validates a whitelisted investor" $
    integrationalTestExpectation $ do
      let investors = [investor1, investor2]
      dsAddr <- originateDSToken
      fwd <- originateValidatedForwarder dsAddr investors
      forM_ investors $ \inv ->
        lCallDef fwd . RightParameter $ Validate inv
      validate . Right $ expectAnySuccess

  it "Fails to validate a non-whitelisted investor" $
    integrationalTestExpectation $ do
      let investors = [investor1, investor2]
      dsAddr <- originateDSToken
      fwd <- originateValidatedForwarder dsAddr investors
      lCallDef fwd . RightParameter $ Validate investor3
      validate . Left $ lExpectError (== [mt|not in whitelist|])

  it "Returns a list of whitelisted investors" $
    integrationalTestExpectation $ do
      let investors = [investor1, investor2]
      dsAddr <- originateDSToken
      fwd <- originateValidatedForwarder dsAddr investors
      consumer <- lOriginateEmpty @Whitelist contractConsumer "consumer"
      lCallDef fwd . RightParameter . GetWhitelist $ mkView () consumer
      validate . Right $
        lExpectViewConsumerStorage consumer $ [Set.fromList investors]

instance Buildable (Set InvestorId) where
  build = listF
