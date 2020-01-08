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
import Michelson.Typed.Instr (Instr)
import Michelson.Typed.Value
import Michelson.Typed.T
import Michelson.Text

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

import Data.Type.Equality
import Data.Typeable
import Prelude (Enum(..), Eq(..), ($), String, show, id)
import Data.Singletons (SingI)

instance IsoValue (Value' Instr a) where
  type ToT (Value' Instr a) = a
  toVal = id
  fromVal = id

-- | The number of sub-tokens to forward
type Parameter = Natural

-- | We have the addresses of:
-- - The sub-token contract, assumed to accept `DS.Parameter`
-- - The central wallet to transfer sub-tokens to
type Storage = ()


instance (SingI ct, Typeable ct) => ParameterEntryPoints (Value ('Tc ct)) where
  parameterEntryPoints = pepNone

instance ParameterEntryPoints Natural where
  parameterEntryPoints = pepNone

toTransferParameter :: forall s. Address & Natural & s :-> ManagedLedger.Parameter & s
toTransferParameter = do
  pair
  self @Parameter
  address
  pair
  coerce_ @(Address, (Address, Natural)) @ManagedLedger.TransferParams
  wrap_ #cTransfer

-- changed
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

contractOverValue :: forall cp st. Contract cp st -> Contract (Value (ToT cp)) (Value (ToT st))
contractOverValue x = coerce_ # x # coerce_

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

