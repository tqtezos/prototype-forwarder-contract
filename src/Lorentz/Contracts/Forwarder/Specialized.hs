{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.Specialized where

import Lorentz hiding (SomeContract(..))
import Lorentz.Run (analyzeLorentz)
import Lorentz.Base (SomeContract(..))
import Michelson.Analyzer (AnalyzerRes)
import Michelson.Text

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

import Data.Type.Equality
import Data.Typeable
import Prelude (Enum(..), Eq(..), ($), String, show)

import GHC.Natural.Orphans ()


-- | The number of sub-tokens to forward
type Parameter = Natural

-- | We have the addresses of:
-- - The sub-token contract, assumed to accept `DS.Parameter`
-- - The central wallet to transfer sub-tokens to
type Storage = ()


toTransferParameter :: forall s. Address & Natural & s :-> ManagedLedger.Parameter & s
toTransferParameter = do
  pair
  self @Parameter
  address
  pair
  forcedCoerce_ @(Address, (Address, Natural)) @ManagedLedger.TransferParams
  wrap_ #cTransfer

runSpecializedTransfer :: Address -> ContractRef ManagedLedger.Parameter -> (Natural & s) :-> (Operation & s)
runSpecializedTransfer centralWalletAddr' (ContractRef contractAddr' _) = do
  push centralWalletAddr'
  toTransferParameter
  dip $ do
    push contractAddr'
    contract @ManagedLedger.Parameter
    ifNone
      (failUnexpected (mkMTextUnsafe "not FA1.2"))
      (push (toEnum 0 :: Mutez))
  transferTokens

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
specializedForwarderContract :: Address -> ContractRef ManagedLedger.Parameter -> Contract Parameter Storage
specializedForwarderContract centralWalletAddr' contractAddr' = do
  car
  runSpecializedTransfer centralWalletAddr' contractAddr'
  dip nil
  cons
  dip unit
  pair

analyzeSpecializedForwarder :: Address -> ContractRef ManagedLedger.Parameter -> AnalyzerRes
analyzeSpecializedForwarder centralWalletAddr' contractAddr' =
  analyzeLorentz $ specializedForwarderContract centralWalletAddr' contractAddr'

-- | `forcedCoerce_` to convert parameter and storage types to their `Value` equivalents
contractOverValue :: forall cp st. Contract cp st -> Contract (Value (ToT cp)) (Value (ToT st))
contractOverValue x = forcedCoerce_ # x # forcedCoerce_

-- | Verify that `SomeContract` is an instance of `specializedForwarderContract`, for some
-- particular central wallet address and DS Token address.
verifyForwarderContract :: Address -> ContractRef ManagedLedger.Parameter -> SomeContract -> Either String ()
verifyForwarderContract centralWalletAddr' dsTokenContractRef' (SomeContract (contract' :: Contract cp st)) =
  case eqT @(ToT cp) @(ToT Parameter) of
    Nothing -> Left $ "Unexpected parameter type: " <> show (typeRep (Proxy @(ToT cp)))
    Just Refl ->
      case eqT @(ToT st) @(ToT Storage) of
        Nothing -> Left $ "Unexpected storage type: " <> show (typeRep (Proxy @(ToT st)))
        Just Refl ->
          let givenContract =
                printLorentzContract
                  forceOneline $
                  contractOverValue contract'
           in case givenContract == expectedContract of
                True -> return ()
                False -> Left "The contracts have the same type, but different implementations"
  where
    forceOneline = True
    expectedContract =
      printLorentzContract
           forceOneline
           (specializedForwarderContract
              centralWalletAddr'
              dsTokenContractRef')

