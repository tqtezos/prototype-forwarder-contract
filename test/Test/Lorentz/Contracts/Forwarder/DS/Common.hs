{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module Test.Lorentz.Contracts.Forwarder.DS.Common where

#ifdef HAS_DSTOKEN

import Control.Monad (forM_)
import Lorentz

import Data.Coerce (coerce)
import GHC.TypeLits (KnownSymbol)

import qualified Lorentz.Contracts.DS.Permanent as Permanent
import Lorentz.Contracts.DS.Preprocess
import Lorentz.Contracts.DS.V0 as V0
import Lorentz.Contracts.DS.V1 as V1
import qualified Lorentz.Contracts.DS.V1.Token as Token
import Lorentz.Contracts.DS.V1.Registry as Registry
import qualified Lorentz.Contracts.Upgradeable.Common as Upg
import Lorentz.Test
import Lorentz.UParam

import Test.Lorentz.Contracts.Forwarder.Common

investor1, investor2 :: Registry.InvestorId
investor1 = Registry.InvestorId [mt|investor1|]
investor2 = Registry.InvestorId [mt|investor2|]
investor3 = Registry.InvestorId [mt|investor3|]

maxLocksPerInvestorDefault :: Natural
maxLocksPerInvestorDefault = 100

defaultOrigParameters :: OriginationParameters
defaultOrigParameters = OriginationParameters
  { opMaster = masterAddress
  , opTokenMeta = Token.dummyTokenMeta
  , opComplianceInfo = weakestComplianceInfo
  , opMaxLocksPerInvestor = maxLocksPerInvestorDefault
  }

dsCall
  :: forall a name.
  ( NicePackedValue a
  , KnownSymbol name
  , RequireUniqueEntryPoints V1.Interface
  , LookupEntryPoint name V1.Interface ~ a
  )
  => Label name
  -> DSRef
  -> a
  -> IntegrationalScenarioM ()
dsCall method addr arg =
  lCallDef addr $
  Upg.Run (mkUParam method arg :: UParam V1.Interface)

-- | 'TAddress' of DS protocol contract (V1).
type DSRef = TAddress V1.Parameter

-- | Originate V1 of DS Protocol with default parameters.
originateDSToken :: IntegrationalScenarioM DSRef
originateDSToken =
  originateDSTokenWithParameters defaultOrigParameters

-- | Originate V1 of DS Protocol with custom parameters.
originateDSTokenWithParameters
  :: OriginationParameters -> IntegrationalScenarioM DSRef
originateDSTokenWithParameters origParameters = do
  contract <- lOriginate
    v0Contract
    "DS Protocol"
    (Permanent.mkEmptyStorage masterAddress)
    (toMutez 0)
  upgradeToV1 origParameters contract

upgradeToV1 ::
     OriginationParameters
  -> TAddress (Upg.Parameter V0.VersionId0)
  -> IntegrationalScenarioM DSRef
upgradeToV1 origParameters contract =
  -- 'coerceContractRef' is necessary because our 'Parameter' is a
  -- newtype wrapper.
  coerce <$>
  Upg.integrationalTestEpwUpgrade (v1UpgradeParameters origParameters) contract

registerInvestor
    :: DSRef -> InvestorId -> [Address] -> IntegrationalScenarioM ()
registerInvestor dsRef investor addressList = do
  withSender masterAddress $
    dsCall #callRegistry dsRef $ RegisterInvestor investor
  forM_ addressList $ \address -> do
    withSender masterAddress $
      dsCall #callRegistry dsRef $
        AddWallet (investor, WalletId address)

mint :: DSRef -> Address -> Natural -> IntegrationalScenarioM ()
mint dsRef beneficiary amount =
  withSender masterAddress $
    dsCall #callTokenMint dsRef $
      Token.Mint (Token.mintParamSimple wallet amount, Token.CommitRun)
  where wallet = WalletId beneficiary

#endif
