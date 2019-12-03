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

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.DS.V1.Specialized where

import Lorentz hiding (SomeContract(..))
import Lorentz.Run (analyzeLorentz)
import Michelson.Analyzer (AnalyzerRes)
-- import Michelson.Optimizer
-- import Michelson.Typed.Instr (toFullContract)
import qualified Michelson.Typed.Instr as Instr
-- import Michelson.Typed.Scope
import Michelson.Typed.T
import Michelson.TypeCheck.Types
import Michelson.Text

import qualified Lorentz.Contracts.DS.V1 as DS
import Lorentz.Contracts.Forwarder.DS.V1 (toTransferParameter)

import Data.Type.Equality
import Data.Typeable
import Prelude (Enum(..), Eq(..), ($), String, show)
import Data.Singletons (SingI)


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

-- changed
runSpecializedTransfer :: Address -> ContractRef DS.Parameter -> (Natural & s) :-> (Operation & s)
runSpecializedTransfer centralWalletAddr' (ContractRef contractAddr' _) = do
  push centralWalletAddr'
  toTransferParameter
  dip $ do
    push contractAddr'
    contract @DS.Parameter
    ifNone
      (failUnexpected (mkMTextUnsafe "not DS"))
      (push (toEnum 0 :: Mutez))
  transferTokens

-- | Forwarder contract: forwards the given number of sub-tokens
-- from its own address to the central wallet.
specializedForwarderContract :: Address -> ContractRef DS.Parameter -> Contract Parameter Storage
specializedForwarderContract centralWalletAddr' contractAddr' = do
  car
  runSpecializedTransfer centralWalletAddr' contractAddr'
  dip nil
  cons
  dip unit
  pair

-- -- specializedForwarderCompilationWay :: LorentzCompilationWay Parameter Storage
-- specializedForwarderCompilationWay :: (KnownValue cp, NoOperation cp, NoBigMap cp, ProperStorageBetterErrors (ToT st), IsoValue st)
--                                    => LorentzCompilationWay cp st
-- specializedForwarderCompilationWay =
--   LorentzCompilationWay (toFullContract . optimize . compileLorentzContract)

-- forwarderDocumentation :: LText

analyzeSpecializedForwarder :: Address -> ContractRef DS.Parameter -> AnalyzerRes
analyzeSpecializedForwarder centralWalletAddr' contractAddr' =
  analyzeLorentz $ specializedForwarderContract centralWalletAddr' contractAddr'

makeI :: Instr.Contract a b -> Contract (Value a) (Value b)
makeI = I

verifyForwarderContract :: Address -> ContractRef DS.Parameter -> SomeContract -> Either String ()
verifyForwarderContract centralWalletAddr' dsTokenContractRef' (SomeContract (contract' :: Instr.Contract cp st) _ _) =
  case eqT @cp @(ToT Parameter) of
    Nothing -> Left $ "Unexpected parameter type: " <> show (typeRep (Proxy @cp))
    Just Refl ->
      case eqT @st @(ToT Storage) of
        Nothing -> Left $ "Unexpected storage type: " <> show (typeRep (Proxy @st))
        Just Refl ->
          let givenContract =
                printLorentzContract
                  forceOneline
                  -- specializedForwarderCompilationWay
                  (makeI contract')
           in case givenContract == expectedContract of
                True -> return ()
                False -> Left "The contracts have the same type, but different implementations"
  where
    forceOneline = True
    expectedContract =
      printLorentzContract
           forceOneline
           -- specializedForwarderCompilationWay
           (specializedForwarderContract
              centralWalletAddr'
              dsTokenContractRef')

