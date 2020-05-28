{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module NettestHelpers
  ( genContractId

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

import qualified Lorentz.Contracts.Forwarder.Specialized as Fwd
import qualified Lorentz.Contracts.Upgradeable.Common as Upg
import Morley.Nettest
  (AddrOrAlias(..), NettestClientConfig(..), NettestScenario, NettestT, OriginateData(..))
import qualified Morley.Nettest as NT

genContractId :: Text -> IO Text
genContractId prefix = do
  contractIdNum <- randomRIO @Int (1, 10000)
  return (prefix <> show contractIdNum)

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

