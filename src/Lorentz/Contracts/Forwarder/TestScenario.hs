{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Lorentz.Contracts.Forwarder.TestScenario
  ( runTestScenario
  , TestScenarioParameters(..)
  ) where

import GHC.TypeLits (KnownSymbol)
import Universum (safeHead, (?:))

import qualified Lorentz as L
import Lorentz.Constraints
import qualified Lorentz.Contracts.DS.V1 as V1
import Lorentz.Contracts.DS.V1.Compliance.Types
import qualified Lorentz.Contracts.DS.V1.Token as Token
import qualified Lorentz.Contracts.DS.V1.Registry as Registry
import Lorentz.Contracts.DS.V1.Registry.Types
import Lorentz.Contracts.Upgradeable.Common as Upg
import Lorentz.Value (Label)
import Morley.Nettest (AddrOrAlias(..), NettestT)
import Michelson.Text (mt)
import qualified Morley.Nettest as NT
import Tezos.Address (Address)
import Tezos.Core

inv0, inv1 :: InvestorId
inv0 = InvestorId [mt|r1|]
inv1 = InvestorId [mt|r2|]

country0, euCountry :: Country
country0 = Country [mt|country|]
euCountry = safeHead euCountries ?: error "no EU countries"

attrInfo0 :: AttributeInfo
attrInfo0 = AttributeInfo AttrStatApproved Nothing Nothing

mint :: WalletId -> L.Natural -> Token.ParameterMint
mint wallet val =
  Token.Mint (Token.mintParamSimple wallet val, Token.CommitRun)

burn :: WalletId -> L.Natural -> Token.ParameterBurn
burn wallet val =
  Token.Burn (Token.burnParamSimple wallet val, Token.CommitRun)


-- | Required for achieveing worst-case gas consumption for @transfer@.
-- This does not include 'ciForceAccredited' enabling
ciMostChecksForTransfer :: SetComplianceInfoData
ciMostChecksForTransfer = ComplianceInfoExt
  { ciTotalInvestorLimit = Just $ MaybeInfinity 999
  , ciMinUSTokens = Nothing
  , ciMinEUTokens = Nothing
  , ciUSInvestorLimit = Just $ MaybeInfinity 999
  , ciUSAccreditedInvestorsLimit = Just $ MaybeInfinity 999
  , ciNonAccreditedInvestorsLimit = Just $ MaybeInfinity 999
  , ciMaxUSInvestorPercentage = Nothing
  , ciFlowbackBlockExpiryDate = Just $ MaybeInfinity farFuture
  , ciUSLockPeriod = Nothing
  , ciNonUSLockPeriod = Nothing
  , ciMinimumTotalInvestors = Nothing
  , ciMinimumHoldingsPerInvestor = Nothing
  , ciMaximumHoldingsPerInvestor = Just $ MaybeInfinity 10e18
  , ciEURetailLimit = Just $ MaybeInfinity 999
  , ciForceFullTransfer = Just True
  , ciForceAccredited = Just True
  , ciForceAccreditedUS = Just True
  }

mkRun
  :: ( KnownSymbol name
     , NicePackedValue arg
     , L.LookupEntryPoint name (Upg.VerInterface ver) ~ arg
     , L.RequireUniqueEntryPoints (Upg.VerInterface ver)
     )
  => Label name -> arg -> Upg.Parameter ver
mkRun name arg = Upg.Run $ L.mkUParam name arg

mkRunV1
  :: ( KnownSymbol name
     , NicePackedValue arg
     , L.LookupEntryPoint name (Upg.VerInterface ver) ~ arg
     , ver ~ V1.VersionId1
     )
  => Label name -> arg -> Upg.Parameter ver
mkRunV1 = mkRun

-- | 'NT.TransferData' for the default entrypoint of some contract.
defCallData
  :: forall cp. (Show cp, NiceParameterFull cp)
  => Address -> (Address, cp) -> NT.TransferData
defCallData contract (from, param) =
  NT.TransferData
    { tdFrom = AddrResolved from
    , tdTo = AddrResolved contract
    , tdAmount = 0
    , tdEntrypoint = L.DefEpName
    , tdParameter = param
    }

type Scenario = [NT.TransferData]

compileScenario
  :: forall m. Monad m
  => NettestT m Scenario -> NettestT m ()
compileScenario scenarioM = do
  scenario <- scenarioM
  mapM_ NT.transfer scenario

data TestScenarioParameters
  = TestScenarioParameters
    { tspDSToken :: Address
    , tspCentralWallet :: Address
    , tspForwarder :: Address
    }

testScenario :: Monad m => TestScenarioParameters -> NettestT m Scenario
testScenario TestScenarioParameters{..} = do
  master <- NT.resolveNettestAddr
  someone <- NT.newAddress "addr1"

  let
    ds = tspDSToken
    fwd = tspForwarder
    fwdWallet = WalletId fwd
    centralWallet = WalletId tspCentralWallet

    forward :: L.Natural -> NT.TransferData
    forward tokens = defCallData fwd (someone, tokens)

    dsCallData
      :: (Address, Upg.Parameter V1.VersionId1)
      -> NT.TransferData
    dsCallData = defCallData ds

    performTransfer :: Scenario
    performTransfer =
      let tokens = 10e15 in
      [ dsCallData (master, mkRunV1 #callTokenMint (mint fwdWallet tokens))
      , forward tokens
      , dsCallData (master, mkRunV1 #callTokenBurn (burn centralWallet tokens))
      ]

    initRegistryMinimal :: Scenario
    initRegistryMinimal =
      map dsCallData
      [ (master, mkRunV1 #callRegistry (Registry.RegisterInvestor inv0))
      , (master, mkRunV1 #callRegistry (Registry.AddWallet (inv0, fwdWallet)))
      , (master, mkRunV1 #callRegistry (Registry.RegisterInvestor inv1))
      , (master, mkRunV1 #callRegistry (Registry.AddWallet (inv1, centralWallet)))
      ]

  pure $ mconcat
    [ initRegistryMinimal
    , performTransfer
    ]

runTestScenario
  :: Monad m
  => TestScenarioParameters -> NettestT m ()
runTestScenario params = compileScenario $ testScenario params
