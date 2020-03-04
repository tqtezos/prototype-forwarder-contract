{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring where

#ifdef HAS_DSTOKEN

import Data.String (IsString(..))

import Lorentz
import Michelson.Text (mkMTextUnsafe)
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
mkStorage :: [InvestorId] -> Timestamp -> Storage
mkStorage whitelist' expirationDate' =
  flip Expiring.Storage expirationDate' $
    () :&: ValidateReception.mkStorage whitelist'

-- | Convenient `Storage` constructor, that also parses `InvestorId`'s
mkStorageWithInvestorIds :: [String] -> Timestamp -> Storage
mkStorageWithInvestorIds whitelist' =
  mkStorage $ InvestorId . mkMTextUnsafe . fromString <$> whitelist'

-- | A contract that:
-- - Expires after the given timestamp in `Expiring.Storage`
-- - Offers a `Forwarder.specializedForwarderContract` interface
-- - Offers a `ValidateReception.validateReceptionContract` interface
validatedExpiringForwarderContract :: Address -> ContractRef DS.Parameter -> Contract Parameter Storage
validatedExpiringForwarderContract centralWalletAddr' contractAddr' =
  Expiring.expiringContract $
  productContract
    (Forwarder.specializedForwarderContract centralWalletAddr' contractAddr')
    ValidateReception.validateReceptionContract

#endif
