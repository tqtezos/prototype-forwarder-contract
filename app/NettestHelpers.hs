{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module NettestHelpers
  ( genContractId

  #ifdef HAS_DSTOKEN
  , originateDS
  #endif

  , originateForwarder
  ) where

import Prelude hiding (putStrLn, show, print)

import Control.Monad (forM_)
import Fmt (pretty)
import System.Random (randomRIO)
import Universum.Print
import Universum.String

import Lorentz (mt, Text, Address)
import qualified Lorentz as L

#ifdef HAS_DSTOKEN
import Lorentz.Contracts.DS.Permanent (mkEmptyStorage)
import Lorentz.Contracts.DS.Preprocess (v0Contract, v1CompiledMigration, v1UpgradeParameters)
import qualified Lorentz.Contracts.DS.V1 as V1
import qualified Lorentz.Contracts.DS.V1.Token as Token
#endif

import qualified Lorentz.Contracts.Forwarder.Specialized as Fwd
import qualified Lorentz.Contracts.Upgradeable.Common as Upg
import Morley.Nettest
  (AddrOrAlias(..), NettestClientConfig(..), NettestScenario, NettestT, OriginateData(..))
import qualified Morley.Nettest as NT

genContractId :: Text -> IO Text
genContractId prefix = do
  contractIdNum <- randomRIO @Int (1, 10000)
  return (prefix <> show contractIdNum)

#ifdef HAS_DSTOKEN
mkProductionOrigParams :: L.Address -> V1.OriginationParameters
mkProductionOrigParams master = V1.OriginationParameters
  { opMaster = master
  , opTokenMeta = Token.TokenMeta
    { tmName = [mt|DS Token|]
    , tmSymbol = [mt|DS|]
    , tmDecimals = 18
    }
  , opComplianceInfo = V1.productionComplianceInfo
  , opMaxLocksPerInvestor = 100
  }

originateDS :: Monad m => Text -> NettestT m Address
originateDS contractId = do
  master <- NT.resolveNettestAddr
  let origParameters = mkProductionOrigParams master

  contract <- NT.originate OriginateData
    { odFrom = AddrResolved master
    , odName = contractId
    , odBalance = 0
    , odStorage = mkEmptyStorage master
    , odContract = v0Contract
    }
  forM_ (Upg.makeEpwUpgrade $ v1UpgradeParameters origParameters) $
    NT.callFrom (AddrResolved master) (AddrResolved contract) L.DefEpName
  NT.comment $
    "Successfully deployed DSToken: " <>
    pretty contractId <> " / " <> pretty contract
  return contract
#endif

originateForwarder
  :: Monad m => Text -> Address -> Address -> NettestT m Address
originateForwarder contractId ds centralWallet = do
  master <- NT.resolveNettestAddr
  contract <- NT.originate OriginateData
    { odFrom = AddrResolved master
    , odName = contractId
    , odBalance = 0
    , odStorage = ()
    , odContract = Fwd.specializedForwarderContract centralWallet ds
    }
  NT.comment $
    "Successfully deployed forwarder: " <>
    pretty contractId <> " / " <> pretty contract
  return contract


