{-# LANGUAGE TypeOperators #-}

module Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring where

import Lorentz
import qualified Lorentz.Contracts.Expiring as Expiring
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as Forwarder
import qualified Lorentz.Contracts.Validate.Reception as ValidateReception

import Lorentz.Contracts.Product

import Lorentz.Contracts.DS.V1.Registry.Types (InvestorId(..))
import qualified Lorentz.Contracts.DS.V1 as DS


-- | Expiring, accepts either
-- a `Forwarder.Parameter` or a `ValidateReception.Parameter`
type Parameter = Expiring.Parameter (Forwarder.Parameter :|: ValidateReception.Parameter)

-- | Expiring, holds both the storage for
-- `Forwarder.Storage` and `ValidateReception.Storage`
type Storage = Expiring.Storage (Forwarder.Storage :&: ValidateReception.Storage)

-- | Convenient `Storage` constructor
mkStorage :: Address -> [InvestorId] -> Natural -> Timestamp -> Storage
mkStorage dsTokenAddress' whitelist' tokenLimit' expirationDate' =
  flip Expiring.Storage expirationDate' $
    () :&: ValidateReception.mkStorage dsTokenAddress' whitelist' tokenLimit'

-- | A contract that:
-- - Expires after the given timestamp in `Expiring.Storage`
-- - Offers a `Forwarder.specializedForwarderContract` interface
-- - Offers a `ValidateReception.validateReceptionContract` interface
validatedExpiringForwarder :: Address -> ContractRef DS.Parameter -> Contract Parameter Storage
validatedExpiringForwarder centralWalletAddr' contractAddr' =
  Expiring.expiringContract $
  productContract
    (Forwarder.specializedForwarderContract centralWalletAddr' contractAddr')
    ValidateReception.validateReceptionContract

