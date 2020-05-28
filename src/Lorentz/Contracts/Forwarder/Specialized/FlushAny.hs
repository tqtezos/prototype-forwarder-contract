{-# LANGUAGE LambdaCase #-}
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

module Lorentz.Contracts.Forwarder.Specialized.FlushAny where

import Lorentz hiding (SomeContract(..))
import Lorentz.Run (analyzeLorentz)
import Lorentz.Base (SomeContract(..))
import Michelson.Analyzer (AnalyzerRes)
import Michelson.Typed.EntryPoints

import Lorentz.Contracts.Spec.AbstractLedgerInterface (TransferParams)
import qualified Lorentz.Contracts.Spec.AbstractLedgerInterface as AL
import qualified Lorentz.Contracts.Forwarder.Specialized as Specialized

import Data.Type.Equality
import Data.Typeable
import Prelude (Show(..), Enum(..), Eq(..), ($), String, show)

import Michelson.Typed.Value.Orphans ()


-- | The number of sub-tokens to forward and the typed contract address of the
-- token to forward it on
data Parameter = Parameter
  { amountToFlush :: !Natural
  , tokenContract :: !(ContractRef TransferParams)
  }
  deriving stock Show
  deriving stock Generic
  deriving anyclass IsoValue

instance HasTypeAnn Parameter

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdNone

-- | Unwrap a `Parameter`
unParameter :: Parameter & s :-> (Natural, ContractRef TransferParams) & s
unParameter = forcedCoerce_

-- | Assumes the `Address` points to a `AL.Parameter`
mkParameter :: Natural -> Address -> Parameter
mkParameter amountToFlush' tokenContract' =
  Parameter
    amountToFlush' $
    ContractRef tokenContract' unsafeEPCall
  where
    epParamNotes' :: ParamNotes (ToT AL.Parameter)
    epParamNotes' =
      ParamNotesUnsafe $
      epdNotes @(ParameterEntryPointsDerivation AL.Parameter) @AL.Parameter

    mkEPCallRes' :: MkEntryPointCallRes (ToT AL.Parameter)
    mkEPCallRes' =
      case mkEntryPointCall @(ToT AL.Parameter)
        (EpNameUnsafe "transfer")
        epParamNotes' of
        Nothing -> error "mkParameter: TransferParams does not have label 'transfer'"
        Just xs -> xs

    unsafeEPCall :: SomeEntryPointCall TransferParams
    unsafeEPCall =
      (\case
        MkEntryPointCallRes _ (epCall' :: EntryPointCallT (ToT AL.Parameter) arg) ->
          case eqT @arg @(ToT TransferParams) of
            Nothing -> error "mkParameter: arg does not have type TransferParams"
            Just Refl ->
              SomeEpc @(ToT TransferParams) @(ToT AL.Parameter) epCall'
      ) mkEPCallRes'


-- | We have the addresses of:
-- - The central wallet to transfer sub-tokens to
type Storage = ()

-- | Wrap a @to@ `Address` and number of tokens to transfer
-- in `TransferParams`, sending from `self`
toTransferParameter :: forall s. Address & Natural & s :-> TransferParams & s
toTransferParameter = do
  pair
  self @Parameter
  address
  pair
  forcedCoerce_ @(Address, (Address, Natural)) @TransferParams

-- | Run a transfer to the given central wallet `Address`, given the token
-- contract `Address` and the number of tokens to transfer
runSpecializedAnyTransfer :: Address -> (Natural & ContractRef TransferParams & s) :-> (Operation & s)
runSpecializedAnyTransfer centralWalletAddr' = do
  push centralWalletAddr'
  toTransferParameter
  dip . push $ toEnum @Mutez 0
  transferTokens

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
specializedAnyForwarderContract :: Address -> ContractCode Parameter Storage
specializedAnyForwarderContract centralWalletAddr' = do
  car
  unParameter
  unpair
  runSpecializedAnyTransfer centralWalletAddr'
  dip nil
  cons
  dip unit
  pair

-- | `analyzeLorentz` specialized to the `specializedAnyForwarderContract`
analyzeSpecializedAnyForwarder :: Address -> AnalyzerRes
analyzeSpecializedAnyForwarder centralWalletAddr' =
  analyzeLorentz $ specializedAnyForwarderContract centralWalletAddr'

-- | Verify that `SomeContract` is an instance of `specializedAnyForwarderContract`, for some
-- particular central wallet address and token address.
verifyForwarderContract :: Address -> SomeContract -> Either String ()
verifyForwarderContract centralWalletAddr' (SomeContract (contract' :: ContractCode cp st)) =
  case eqT @(ToT cp) @(ToT Parameter) of
    Nothing -> Left $ "Unexpected parameter type: " <> show (typeRep (Proxy @(ToT cp)))
    Just Refl ->
      case eqT @(ToT st) @(ToT Storage) of
        Nothing -> Left $ "Unexpected storage type: " <> show (typeRep (Proxy @(ToT st)))
        Just Refl ->
          let givenContract =
                printLorentzContract
                  forceOneline $
                  Specialized.contractOverValue contract'
           in case givenContract == expectedContract of
                True -> return ()
                False -> Left "The contracts have the same type, but different implementations"
  where
    forceOneline = True
    expectedContract =
      printLorentzContract
           forceOneline
           (specializedAnyForwarderContract
              centralWalletAddr')

