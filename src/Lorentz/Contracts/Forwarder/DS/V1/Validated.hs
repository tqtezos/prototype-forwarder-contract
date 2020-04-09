{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Lorentz.Contracts.Forwarder.DS.V1.Validated where

#ifdef HAS_DSTOKEN

import Data.String (IsString(..))

import Lorentz
import Michelson.Text (mkMTextUnsafe)
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as Forwarder
import qualified Lorentz.Contracts.Validate.Reception as ValidateReception

import Lorentz.Contracts.Product

import Lorentz.Contracts.DS.V1.Registry.Types (InvestorId(..))
import qualified Lorentz.Contracts.DS.V1 as DS


-- | Accepts either
-- a `Forwarder.Parameter` or a `ValidateReception.Parameter`
type Parameter = Forwarder.Parameter :|: ValidateReception.Parameter

-- | Both the storage for
-- `Forwarder.Storage` and `ValidateReception.Storage`
type Storage = Forwarder.Storage :&: ValidateReception.Storage

-- | Convenient `Storage` constructor
mkStorage :: [InvestorId] -> Storage
mkStorage whitelist' =
  () :&: ValidateReception.mkStorage whitelist'

-- | Convenient `Storage` constructor, that also parses `InvestorId`'s
mkStorageWithInvestorIds :: [String] -> Storage
mkStorageWithInvestorIds whitelist' =
  mkStorage $ InvestorId . mkMTextUnsafe . fromString <$> whitelist'

-- | A contract that:
-- - Offers a `Forwarder.specializedForwarderContract` interface
-- - Offers a `ValidateReception.validateReceptionContract` interface
validatedForwarderContract :: Address -> ContractRef DS.Parameter -> ContractCode Parameter Storage
validatedForwarderContract centralWalletAddr' contractAddr' =
  productContract
    (Forwarder.specializedForwarderContract centralWalletAddr' contractAddr')
    ValidateReception.validateReceptionContract

#endif
