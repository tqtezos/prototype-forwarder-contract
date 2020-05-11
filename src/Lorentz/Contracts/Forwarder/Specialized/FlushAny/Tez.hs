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

module Lorentz.Contracts.Forwarder.Specialized.FlushAny.Tez where

import Lorentz hiding (SomeContract(..))
import Lorentz.Run (analyzeLorentz)
import Lorentz.Base (SomeContract(..))
import Michelson.Analyzer (AnalyzerRes)
import Michelson.Text

import qualified Lorentz.Contracts.Forwarder.Specialized as Specialized
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny as FlushAny

import Data.Singletons
import Data.Type.Equality
import Data.Typeable
import Prelude (Show(..), Enum(..), Eq(..), ($), String, show)

import Michelson.Typed.Value.Orphans ()

-- | We have the addresses of:
-- - The central wallet to transfer sub-tokens to
type Storage = ()


data Parameter = Default () | Flush FlushAny.Parameter
  deriving  (Generic, IsoValue, Show)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

instance (SingI t) => ParameterHasEntryPoints (Value t) where
  type ParameterEntryPointsDerivation (Value t) = EpdNone

runSpecializedAnyTezTransfer :: Address -> (Maybe FlushAny.Parameter & s) :-> ([Operation] & s)
runSpecializedAnyTezTransfer centralWalletAddr' = do
  dip $ push centralWalletAddr'
  ifNone nil $ do
       dip dup
       FlushAny.unParameter
       unpair
       dig @2
       FlushAny.toTransferParameter
       dip . push $ toEnum @Mutez 0
       transferTokens
       dip nil
       cons
  balance
  push $ toEnum @Mutez 0
  ifEq
    (dip drop)
    (do
      dip $ do
        contract @()
        assertSome $ mkMTextUnsafe "not a wallet"
        balance
        unit
        transferTokens
      swap
      cons
    )

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
--
-- It also forwards all held Tez to the central wallet.
specializedAnyTezForwarderContract :: Address -> ContractCode Parameter Storage
specializedAnyTezForwarderContract centralWalletAddr' = do
  car
  caseT @Parameter
    ( #cDefault /-> drop >> none
    , #cFlush /-> some
    )
  runSpecializedAnyTezTransfer centralWalletAddr'
  dip unit
  pair

analyzeSpecializedAnyTezForwarder :: Address -> AnalyzerRes
analyzeSpecializedAnyTezForwarder centralWalletAddr' =
  analyzeLorentz $ specializedAnyTezForwarderContract centralWalletAddr'

-- | Verify that `SomeContract` is an instance of `specializedAnyTezForwarderContract`, for some
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
           (specializedAnyTezForwarderContract
              centralWalletAddr')
